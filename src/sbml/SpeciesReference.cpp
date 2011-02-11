/**
 * @file    SpeciesReference.cpp
 * @brief   Implementation of SimpleSpeciesReference, SpeciesReference,
 *          ModifierSpeciesReference, and ListOfSpeciesReferences. 
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
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
 * ---------------------------------------------------------------------- -->*/


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#ifdef USE_LAYOUT
#include <sbml/annotation/LayoutAnnotation.h>
#endif // USE_LAYOUT

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SpeciesReference.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

SimpleSpeciesReference::SimpleSpeciesReference (unsigned int level, 
                                                unsigned int version) :
   SBase ( level, version )
 , mId     ( "" )
 , mName   ( "" )
 , mSpecies( "" )
{
}


SimpleSpeciesReference::SimpleSpeciesReference (SBMLNamespaces *sbmlns) :
   SBase   (sbmlns  )
 , mId     ( "" )
 , mName   ( "" )
 , mSpecies( "" )
{
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
SimpleSpeciesReference::SimpleSpeciesReference() :
  SBase ()
{
}

/** @endcond */
                          

/*
 * Destroys this SimpleSpeciesReference.
 */
SimpleSpeciesReference::~SimpleSpeciesReference ()
{
}


/*
 * Copy constructor. Creates a copy of this SimpleSpeciesReference.
 */
SimpleSpeciesReference::SimpleSpeciesReference(const SimpleSpeciesReference& orig) :
   SBase     ( orig                    )
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId      = orig.mId;
    mName    = orig.mName;
    mSpecies = orig.mSpecies;
  }
}


/*
 * Assignment operator.
 */
SimpleSpeciesReference& SimpleSpeciesReference::operator=(const SimpleSpeciesReference& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    mId = rhs.mId;
    mName = rhs.mName;
    mSpecies = rhs.mSpecies;
  }

  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * SimpleSpeciesReference (if available).
 */
bool
SimpleSpeciesReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return the id of this SBML object.
 */
const string&
SimpleSpeciesReference::getId () const
{
  return mId;
}


/*
 * @return the name of this SBML object.
 */
const string&
SimpleSpeciesReference::getName () const
{
  return (getLevel() == 1) ? mId : mName;
}


/*
 * @return the species for this SimpleSpeciesReference.
 */
const string&
SimpleSpeciesReference::getSpecies () const
{
  return mSpecies;
}


/*
 * @return true if the id of this SBML object is set, false
 * otherwise.
 */
bool
SimpleSpeciesReference::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return true if the name of this SBML object is set, false
 * otherwise.
 */
bool
SimpleSpeciesReference::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/*
 * @return true if the species for this SimpleSpeciesReference is
 * set, false otherwise.
 */
bool
SimpleSpeciesReference::isSetSpecies () const
{
  return (mSpecies.empty() == false);
}


/*
 * Sets the species of this SimpleSpeciesReference to a copy of sid.
 */
int
SimpleSpeciesReference::setSpecies (const std::string& sid)
{
  if (&(sid) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidSBMLSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpecies = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the id of this SBML object to a copy of sid.
 */
int
SimpleSpeciesReference::setId (const std::string& sid)
{
  if (&(sid) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (getLevel() == 1 ||
    (getLevel() == 2 && getVersion() == 1))
  {
#ifdef USE_LAYOUT
    mId = sid;
    return LIBSBML_OPERATION_SUCCESS;
#else
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
#endif
  }
  else if (!(SyntaxChecker::isValidSBMLSId(sid)))
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
SimpleSpeciesReference::setName (const std::string& name)
{
  if (&(name) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (getLevel() == 1 ||
    (getLevel() == 2 && getVersion() == 1))
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (!(SyntaxChecker::isValidSBMLSId(name)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    if (getLevel() == 1) mId = name;
    else mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Unsets the id of this SBML object.
 */
int
SimpleSpeciesReference::unsetId ()
{
  mId.erase();

  if (mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the name of this SBML object.
 */
int
SimpleSpeciesReference::unsetName ()
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
 * @return true if this SpeciesReference is a ModiferSpeciesReference,
 * false otherwise.
 */
bool
SimpleSpeciesReference::isModifier () const
{
  return (getTypeCode() == SBML_MODIFIER_SPECIES_REFERENCE);
}


/** @cond doxygen-libsbml-internal */
bool 
SimpleSpeciesReference::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (!isSetSpecies())
    allPresent = false;

  return allPresent;
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SimpleSpeciesReference::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  switch (level)
  {
  case 1:
    readL1Attributes(attributes);
    break;
  case 2:
    readL2Attributes(attributes);
    break;
  case 3:
  default:
    readL3Attributes(attributes);
    break;
  }
}


void
SimpleSpeciesReference::readL1Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  const string s = (level == 1 && version == 1) ? "specie" : "species";
  expectedAttributes.push_back(s);
  expectedAttributes.push_back("stoichiometry");
  expectedAttributes.push_back("denominator");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        if (isModifier())
          logUnknownAttribute(name, level, version, "<modifierSpeciesReference>");
        else
          logUnknownAttribute(name, level, version, "<speciesReference>");
      }
    }
  }


  //
  // specie : SName   { use="required" }  (L1v1)
  // species: SName   { use="required" }  (L1v2, L2v1->)
  //
  attributes.readInto(s , mSpecies, getErrorLog(), true);
}


void
SimpleSpeciesReference::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("species");
  expectedAttributes.push_back("stoichiometry");
  expectedAttributes.push_back("metaid");
  if (version > 1)
  {
    expectedAttributes.push_back("id");
    expectedAttributes.push_back("name");
    expectedAttributes.push_back("sboTerm");
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        if (isModifier())
          logUnknownAttribute(name, level, version, "<modifierSpeciesReference>");
        else
          logUnknownAttribute(name, level, version, "<speciesReference>");
      }
    }
  }


  //
  // species: SName   { use="required" }  (L1v2, L2v1->)
  //
  attributes.readInto("species" , mSpecies, getErrorLog(), true);
  
  if (version > 1)
  {
    //
    // id: SId  { use="optional" }  (L2v2->)
    //
    bool assigned = attributes.readInto("id", mId, getErrorLog());
    if (assigned && mId.size() == 0)
    {
      logEmptyString("id", level, version, "<speciesReference>");
    }
    if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

    //
    // name: string  { use="optional" }  (L2v2->)
    //
    attributes.readInto("name" , mName);

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2->)
    //
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);
  }

}


void
SimpleSpeciesReference::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("species");
  expectedAttributes.push_back("stoichiometry");
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("id");
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("sboTerm");
  if (!isModifier())
    expectedAttributes.push_back("constant");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        if (isModifier())
          logUnknownAttribute(name, level, version, "<modifierSpeciesReference>");
        else
          logUnknownAttribute(name, level, version, "<speciesReference>");
      }
    }
  }


  //
  // species: SName   { use="required" }  (L1v2, L2v1->)
  //
  bool assigned = attributes.readInto("species" , mSpecies);
  if (!assigned)
  {
    if (isModifier())
      getErrorLog()->logError(AllowedAttributesOnModifier, 
                                                   level, version);
    else
      getErrorLog()->logError(AllowedAttributesOnSpeciesReference, 
                                                   level, version);
  }
 
  //
  // id: SId  { use="optional" }  (L2v2->)
  //
  assigned = attributes.readInto("id", mId, getErrorLog());
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<speciesReference>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // name: string  { use="optional" }  (L2v2->)
  //
  attributes.readInto("name" , mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2->)
  //
  mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SimpleSpeciesReference::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();


  if (level > 1)
  {
    if (!(level == 2 && version == 1))
    {
      //
      // id: SId  { use="optional" }  (L2v2->)
      //
      stream.writeAttribute("id" , mId);

      //
      // name: string  { use="optional" }  (L2v2->)
      //
      stream.writeAttribute("name" , mName);

      //
      // sboTerm: SBOTerm { use="optional" }  (L2v2->)
      //
      SBO::writeTerm(stream, mSBOTerm);
    }
  }

  //
  // specie : SName   { use="required" }  (L1v1)
  // species: SName   { use="required" }  (L1v2, L2v1->)
  //
  const string s = (level == 1 && version == 1) ? "specie" : "species";
  stream.writeAttribute(s , mSpecies);
}
/** @endcond */



SpeciesReference::SpeciesReference (unsigned int level, unsigned int version) :
   SimpleSpeciesReference( level, version)
 , mStoichiometry        ( 1.0 )
 , mDenominator          ( 1   )
 , mStoichiometryMath    ( NULL   )
 , mConstant             (false)
 , mIsSetConstant        (false)
 , mIsSetStoichiometry   (false)
{

  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  // if level 3 values have no defaults
  if (level == 3)
  {
    mStoichiometry = numeric_limits<double>::quiet_NaN();
  }
 
  // if level 1/2 stoichiometry is set by default
  if (level < 3)
  {
    setStoichiometry(1.0);
  }
}

SpeciesReference::SpeciesReference (SBMLNamespaces *sbmlns) :
   SimpleSpeciesReference( sbmlns )
 , mStoichiometry        ( 1.0 )
 , mDenominator          ( 1   )
 , mStoichiometryMath    ( NULL             )
 , mConstant             (false)
 , mIsSetConstant        (false)
 , mIsSetStoichiometry   (false)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  // if level 3 values have no defaults
  if (sbmlns->getLevel() == 3)
  {
    mStoichiometry = numeric_limits<double>::quiet_NaN();
  }

  // if level 1/2 stoichiometry is set by default
  if (sbmlns->getLevel() < 3)
  {
    setStoichiometry(1.0);
  }
}

/** @cond doxygen-libsbml-internal */

/* constructor for validators */
SpeciesReference::SpeciesReference() :
  SimpleSpeciesReference()
{
}


/** @endcond */
 
                          

/*
 * Destroys this SpeciesReference.
 */
SpeciesReference::~SpeciesReference ()
{
  delete mStoichiometryMath;
}


/*
 * Copy constructor. Creates a copy of this SpeciesReference.
 */
SpeciesReference::SpeciesReference (const SpeciesReference& orig) :
   SimpleSpeciesReference( orig                )
 , mStoichiometryMath    ( NULL                   )
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mStoichiometry = orig.mStoichiometry ;
    mDenominator = orig.mDenominator   ;
    mConstant = orig.mConstant;
    mIsSetConstant = orig.mIsSetConstant;
    mIsSetStoichiometry = orig.mIsSetStoichiometry;

    if (orig.mStoichiometryMath != NULL)
    {
      mStoichiometryMath = new StoichiometryMath(*orig.getStoichiometryMath());
      mStoichiometryMath->setParentSBMLObject(this);
    }
  }
}


/*
 * Assignment operator
 */
SpeciesReference& SpeciesReference::operator=(const SpeciesReference& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    this->SimpleSpeciesReference::operator = ( rhs );
    mStoichiometry = rhs.mStoichiometry ;
    mDenominator = rhs.mDenominator   ;
    mConstant = rhs.mConstant;
    mIsSetConstant = rhs.mIsSetConstant;
    mIsSetStoichiometry = rhs.mIsSetStoichiometry;

    delete mStoichiometryMath;
    if (rhs.mStoichiometryMath != NULL)
    {
      mStoichiometryMath = new StoichiometryMath(*rhs.getStoichiometryMath());
      mStoichiometryMath->setParentSBMLObject(this);
    }
    else
    {
      mStoichiometryMath = NULL;
    }
  }

  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * SpeciesReference (if available).
 */
bool
SpeciesReference::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);
  
  if (mStoichiometryMath != NULL) mStoichiometryMath->accept(v);
  
  return result;
}


/*
 * @return a (deep) copy of this SpeciesReference.
 */
SpeciesReference*
SpeciesReference::clone () const
{
  return new SpeciesReference(*this);
}


/*
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 */
void
SpeciesReference::initDefaults ()
{
  //// level 3 has no defaults
  //if (getLevel() < 3)
  //{
    mStoichiometry = 1.0;
    mDenominator   = 1;
  //}
}


/*
 * @return the stoichiometry of this SpeciesReference.
 */
double
SpeciesReference::getStoichiometry () const
{
  return mStoichiometry;
}


/*
 * @return the stoichiometryMath of this SpeciesReference.
 */
const StoichiometryMath*
SpeciesReference::getStoichiometryMath () const
{
  return mStoichiometryMath;
}


/*
 * @return the stoichiometryMath of this SpeciesReference.
 */
StoichiometryMath*
SpeciesReference::getStoichiometryMath ()
{
  return mStoichiometryMath;
}


/*
 * @return the denominator of this SpeciesReference.
 */
int
SpeciesReference::getDenominator () const
{
  return mDenominator;
}


/*
 * Get the value of the "constant" attribute.
 */
bool 
SpeciesReference::getConstant () const
{
  return mConstant;
}


/*
 * @return true if the stoichiometryMath of this SpeciesReference is
 * set, false otherwise.
 */
bool
SpeciesReference::isSetStoichiometryMath () const
{
  return (mStoichiometryMath != NULL);
}


/*
 * @return true if the constant of this SpeciesReference is
 * set, false otherwise.
 */
bool
SpeciesReference::isSetConstant () const
{
  return mIsSetConstant;
}


/*
 * @return true if the stoichiometry of this SpeciesReference is
 * set, false otherwise.
 */
bool
SpeciesReference::isSetStoichiometry () const
{
  return mIsSetStoichiometry;
}


/*
 * Sets the stoichiometry of this SpeciesReference to value.
 */
int
SpeciesReference::setStoichiometry (double value)
{
   unsetStoichiometryMath();

   mStoichiometry = value;
   mIsSetStoichiometry = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the stoichiometryMath of this SpeciesReference to a copy of the
 * given ASTNode.
 */
int
SpeciesReference::setStoichiometryMath (const StoichiometryMath* math)
{
  if ( getLevel() != 2 )
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (mStoichiometryMath == math) 
  {
    mIsSetStoichiometry = false;
    mStoichiometry = 1.0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    return unsetStoichiometryMath();
  }
  else if (getLevel() != math->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != math->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else
  {
    mIsSetStoichiometry = false;
    mStoichiometry      = 1.0;

    delete mStoichiometryMath;
    mStoichiometryMath = static_cast<StoichiometryMath*>(math->clone());
    if (mStoichiometryMath != NULL) mStoichiometryMath->setSBMLDocument(mSBML);
    if (mStoichiometryMath != NULL) 
      mStoichiometryMath->setParentSBMLObject(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the denominator of this SpeciesReference to value.
 */
int
SpeciesReference::setDenominator (int value)
{
  mDenominator = value;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the constant field of this SpeciesReference to value.
 */
int
SpeciesReference::setConstant (bool flag)
{
  if ( getLevel() < 3 )
  {
    mConstant = flag;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mConstant = flag;
    mIsSetConstant = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the "stoichiometryMath" subelement of this SpeciesReference.
 */
int 
SpeciesReference::unsetStoichiometryMath ()
{
  delete mStoichiometryMath;
  mStoichiometryMath = NULL;

  if ( getLevel() != 2 )
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (!mIsSetStoichiometry)
  {
    // 
    // In SBML Level2, "stoichiometry" attribute is set to 1 (default value)
    // if neither the "stoichiometry" attribute and the "stoichiometryMath" 
    // element are present.
    //
    mIsSetStoichiometry = true;
    mStoichiometry = 1.0;
  }

  if (mStoichiometryMath == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/* unset the stoichiometry */
int
SpeciesReference::unsetStoichiometry ()
{
  const int level = getLevel();

  if ( level > 2 )
  {
    mStoichiometry      = numeric_limits<double>::quiet_NaN();
    mIsSetStoichiometry = false;
    if (!isSetStoichiometry())
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
    else
    {
      return LIBSBML_OPERATION_FAILED;
    }
  }
  else
  {
    mStoichiometry      = 1.0;

    if ( level == 2 ) 
    {
      // 
      // In SBML Level2, "stoichiometry" attribute is set to 1 (default value)
      // if neither the "stoichiometry" attribute and the "stoichiometryMath" 
      // element are present.
      //
      if (!isSetStoichiometryMath())
      {
        mIsSetStoichiometry = true;
      }
      else
      {
        mIsSetStoichiometry = false;
      }
    }
    else
    {
      //
      // In SBML Level 1, "stoichiometry" is always set (default is 1.0).
      //
      mIsSetStoichiometry = true;
    }
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Creates a new StoichiometryMath, adds it to this SpeciesReference
 * and returns it.
 */
StoichiometryMath*
SpeciesReference::createStoichiometryMath ()
{
  delete mStoichiometryMath;
  mStoichiometryMath = NULL;

  try
  {
    mStoichiometryMath = new StoichiometryMath(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }

  if (mStoichiometryMath != NULL)
  {
    mStoichiometryMath->setSBMLDocument(mSBML);
    mStoichiometryMath->setParentSBMLObject(this);
  }

  return mStoichiometryMath;
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
SpeciesReference::getTypeCode () const
{
  return SBML_SPECIES_REFERENCE;
}


bool 
SpeciesReference::hasRequiredAttributes() const
{
  bool allPresent = SimpleSpeciesReference::hasRequiredAttributes();

  if (getLevel() > 2 && !isSetConstant())
    allPresent = false;

  return allPresent;
}


/*
 * Sets the annotation of this SBML object to a copy of annotation.
 */
int
SpeciesReference::setAnnotation (const XMLNode* annotation)
{
  int success = SBase::setAnnotation(annotation);

  if (success == 0)
  {
#ifdef USE_LAYOUT
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      // clear existing SBase::mID 
      setId("");

      if(mAnnotation != NULL)
      {
        // parse mAnnotation (if any) and set mId 
        parseSpeciesReferenceAnnotation(mAnnotation,*this);
      }
    }
#endif // USE_LAYOUT
  }

  return success;
}


/*
 * Sets the annotation (by string) of this SBML object to a copy of annotation.
 */
int
SpeciesReference::setAnnotation (const std::string& annotation)
{
  int success = LIBSBML_OPERATION_FAILED;

  if(annotation.empty())
  {
    unsetAnnotation();
    return LIBSBML_OPERATION_SUCCESS;
  }

  XMLNode* annt_xmln;
  if (getSBMLDocument() != NULL)
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln != NULL)
  {
    success = setAnnotation(annt_xmln);
    delete annt_xmln;
  }
  return success;
}


/*
 * Appends annotation to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int
SpeciesReference::appendAnnotation (const XMLNode* annotation)
{
  int success = LIBSBML_OPERATION_FAILED;
  if(annotation == NULL) return LIBSBML_OPERATION_SUCCESS;

  XMLNode* new_annotation = NULL;

#ifdef USE_LAYOUT
  if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
  {
    XMLNode* new_annotation = NULL;
    const string&  name = annotation->getName();

    // check for annotation tags and add if necessary 
    if (name != "annotation")
    {
      XMLToken ann_t = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
      new_annotation = new XMLNode(ann_t);
      new_annotation->addChild(*annotation);
    }
    else
    {
      new_annotation = annotation->clone();
    }

    // parse new_annotation and reset SBase::mId 
    parseSpeciesReferenceAnnotation(new_annotation,*this);

    // delete mId from new_annotation 
//    XMLNode* tmp_annotation=deleteLayoutIdAnnotation(new_annotation);
//    delete new_annotation;
//    new_annotation = tmp_annotation;
  }
#endif // USE_LAYOUT
  success = SBase::appendAnnotation(new_annotation);

  delete new_annotation;

  return success;
}

/*
 * Appends annotation (by string) to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int
SpeciesReference::appendAnnotation (const std::string& annotation)
{
  int success = LIBSBML_OPERATION_FAILED;
  XMLNode* annt_xmln;
  if (getSBMLDocument() != NULL)
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln != NULL)
  {
    success = appendAnnotation(annt_xmln);
    delete annt_xmln;
  }

  return success;
}


/*
 * @return the name of this element ie "speciesReference".
 
 */
const string&
SpeciesReference::getElementName () const
{
  static const string specie  = "specieReference";
  static const string species = "speciesReference";

  return (getLevel() == 1 && getVersion() == 1) ? specie : species;
}


/** @cond doxygen-libsbml-internal */
void
SpeciesReference::sortMath()
{
  if (mStoichiometryMath != NULL && 
    mStoichiometryMath->isSetMath() &&
    mStoichiometryMath->getMath()->isRational())
  {
    mStoichiometry = mStoichiometryMath->getMath()->getNumerator();
    mDenominator   = mStoichiometryMath->getMath()->getDenominator();

    delete mStoichiometryMath;
    mStoichiometryMath = NULL;
  }
}


/*
 * Sets the "stoichiometry" attribute of SBML Level 2 to 1.0 (default)
 * if neither the "stoichiometry" attribute nor the "stoichiometrymath"
 * subelement are present.
 * (This function is internally invoked in SBase::read().)
 */
void 
SpeciesReference::initL2Stoichiometry()
{
  if (getLevel() != 2) return;

  if (!isSetStoichiometry() && !isSetStoichiometryMath())
  {
    setStoichiometry(1.0);
  }
}

/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SpeciesReference::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  
  if (name == "stoichiometryMath")
  {
    if (getLevel() != 2)
    {
      return NULL;
    }
    delete mStoichiometryMath;

    try
    {
      mStoichiometryMath = new StoichiometryMath(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      mStoichiometryMath = new StoichiometryMath(
                                           SBMLDocument::getDefaultLevel(),
                                           SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      mStoichiometryMath = new StoichiometryMath(
                                           SBMLDocument::getDefaultLevel(),
                                           SBMLDocument::getDefaultVersion());
    }

    // clear the default value for stoichiometry
    //
    // unsetStoichiometry();
    //
    // unsetStoichiometry() must not be invoked here to validate
    // an error in which the both of stoichiometry attribute and
    // stoichiometryMath element defined in this speciesreference object. 
    //

    return mStoichiometryMath;
  }
  else
  {
    return NULL;
  }
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
SpeciesReference::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

 // if (name == "stoichiometryMath")
 // {
 //   const XMLToken wrapperElement = stream.next();
 //   stream.skipText();
 //   const XMLToken element = stream.peek();

 //   bool found = false;

 //   /* The first element must always be 'math'. */

 //   if (element.getName() != "math")
 //   {
 //     found = true;
 //   }

 //   /* Check this declares the MathML namespace.  This may be explicitly
 //    * declared here or implicitly declared on the whole document
 //    */

 //   if (!found && element.getNamespaces().getLength() != 0)
 //   {
 //     for (int n = 0; n < element.getNamespaces().getLength(); n++)
 //     {
 //       if (!strcmp(element.getNamespaces().getURI(n).c_str(),
	//	    "http://www.w3.org/1998/Math/MathML"))
 //       {
	//  found = true;
 //         break;
 //       }
 //     }
 //   }
 //   if (!found && mSBML->getNamespaces() != 0)
 //   {
 //     /* check for implicit declaration */
 //     for (int n = 0; n < mSBML->getNamespaces()->getLength(); n++)
 //     {
	//if (!strcmp(mSBML->getNamespaces()->getURI(n).c_str(),
	//	    "http://www.w3.org/1998/Math/MathML"))
	//{
	//  found = true;
	//  break;
	//}
 //     }
 //   }

 //   if (! found)
 //   {
 //     static_cast <SBMLErrorLog*> (stream.getErrorLog())->logError(10201);
 //   }

 //   delete mStoichiometryMath;
 //   mStoichiometryMath = readMathML(stream);
 //   read               = true;

 //   stream.skipPastEnd(wrapperElement);

 //   if (mStoichiometryMath && mStoichiometryMath->isRational())
 //   {
 //     mStoichiometry = mStoichiometryMath->getNumerator();
 //     mDenominator   = mStoichiometryMath->getDenominator();

 //     delete mStoichiometryMath;
 //     mStoichiometryMath = 0;
 //   }
 // }
  //else 

  // This has to do additional work for reading annotations, so the code
  // here is copied and expanded from SBase::readNotes().

  if (name == "annotation")
  {
//    XMLNode* new_annotation = NULL;
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation != NULL)
    {
      if (getLevel() < 3) 
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
	        "Only one <annotation> element is permitted inside a "
	        "particular containing element.");
      }
      else
      {
        logError(MultipleAnnotations, getLevel(), getVersion());
      }
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    if (mCVTerms != NULL)
    {
      unsigned int size = mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
      delete mCVTerms;
    }
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
//    new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
//    delete mAnnotation;
//    mAnnotation = new_annotation;

#ifdef USE_LAYOUT
    // only parse the id annotation if it is Level 1 or Level 2 Version 1
    // everything after Level 2 Version 1 has ids.
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      parseSpeciesReferenceAnnotation(mAnnotation,*this);
      checkAnnotation();

//      new_annotation=deleteLayoutIdAnnotation(mAnnotation);
//      delete mAnnotation;
//      mAnnotation = new_annotation;
    }
#endif // USE_LAYOUT
    read = true;
  }

  return read;
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpeciesReference::readAttributes (const XMLAttributes& attributes)
{
  SimpleSpeciesReference::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  switch (level)
  {
  case 1:
    readL1Attributes(attributes);
    break;
  case 2:
    readL2Attributes(attributes);
    break;
  case 3:
  default:
    readL3Attributes(attributes);
    break;
  }
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpeciesReference::readL1Attributes (const XMLAttributes& attributes)
{

  const int level = getLevel();

  //
  // stoichiometry: integer  { use="optional" default="1" }  (L1v1, L1v2)
  // stoichiometry: double   { use="optional" default="1" }  (L2v1->)
  //
  mIsSetStoichiometry = attributes.readInto("stoichiometry", mStoichiometry);
  if (!mIsSetStoichiometry)
  {
    //  
    // setting default value
    //
    mStoichiometry = 1;
    mIsSetStoichiometry = true;
  }

  //
  // denominator: integer  { use="optional" default="1" }  (L1v1, L1v2)
  //
  attributes.readInto("denominator", mDenominator);

}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpeciesReference::readL2Attributes (const XMLAttributes& attributes)
{
  // stoichiometry: double   { use="optional" default="1" }  (L2v1->)
  //
  mIsSetStoichiometry = attributes.readInto("stoichiometry", mStoichiometry);
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpeciesReference::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = 3;
  const unsigned int version = getVersion();
  //
  // stoichiometry: double   { use="optional" default="1" }  (L2v1->)
  //
  mIsSetStoichiometry = attributes.readInto("stoichiometry", mStoichiometry);

  //
  // constant: bool { use="required" } (L3v1 -> )
  //
  mIsSetConstant = attributes.readInto("constant", mConstant);
  if (!mIsSetConstant && !isModifier())
  {
    logError(AllowedAttributesOnSpeciesReference, level, version);
  }

}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SpeciesReference::writeAttributes (XMLOutputStream& stream) const
{
  SimpleSpeciesReference::writeAttributes(stream);

  if (getLevel() == 1)
  {
    //
    // stoichiometry: integer  { use="optional" default="1" }  (L1v1, L1v2)
    //
    int s = static_cast<int>( mStoichiometry );
    if (s != 1) stream.writeAttribute("stoichiometry", s);

    //
    // denominator  { use="optional" default="1" }  (L1v1, L1v2)
    //
    if (mDenominator != 1) stream.writeAttribute("denominator", mDenominator);
  }
  else if (getLevel() == 2)
  {
    //
    // stoichiometry: double   { use="optional" default="1" }  (L2v1, L2v2)
    //
    if (mStoichiometry != 1 && mDenominator == 1)
    {
      stream.writeAttribute("stoichiometry", mStoichiometry);
    }
  }
  else
  {
    if (isSetStoichiometry())
      stream.writeAttribute("stoichiometry", mStoichiometry);
  }
  //
  // constant: bool { use="required" } (L3v1 -> )
  //
  if (getLevel() > 2)
  {
    stream.writeAttribute("constant", mConstant);
  }
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SpeciesReference::writeElements (XMLOutputStream& stream) const
{
  if ( mNotes != NULL ) stream << *mNotes;
  SpeciesReference * sr = const_cast <SpeciesReference *> (this);
  sr->syncAnnotation();
  if ( mAnnotation != NULL ) stream << *mAnnotation;

  if (getLevel() == 2)
  {
    if (mStoichiometryMath || mDenominator != 1)
    {
      if (mStoichiometryMath != NULL) 
      {
        mStoichiometryMath->write(stream);
      }
      else
      {
        ASTNode node;
        node.setValue(static_cast<long>(mStoichiometry), mDenominator);

        stream.startElement("stoichiometryMath");
        writeMathML(&node, stream);
        stream.endElement("stoichiometryMath");
      }
    }
  }

}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Synchronizes the annotation of this SBML object.
 */
void
SpeciesReference::syncAnnotation ()
{
  SBase::syncAnnotation();

#ifdef USE_LAYOUT
  if (getLevel() == 2)
  {
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      if(mAnnotation != NULL)
      {
        XMLNode* new_annotation = deleteLayoutIdAnnotation(mAnnotation);
        *mAnnotation = *new_annotation;
        delete new_annotation;
      }

      if (this->isSetId())
      {
        XMLNode * idAnnotation = parseLayoutId(this);
        if (idAnnotation != NULL)
        {
          if (mAnnotation == NULL)
          {
            mAnnotation = idAnnotation;
          }
          else
          {
            if (mAnnotation->isEnd())
            {
              mAnnotation->unsetEnd();
            }
            mAnnotation->addChild(idAnnotation->getChild(0));
            delete idAnnotation;
          }
        }

      }
    }
  }
#endif // USE_LAYOUT
}
/** @endcond */


ModifierSpeciesReference::ModifierSpeciesReference (unsigned int level, 
                          unsigned int version) :
  SimpleSpeciesReference(level, version)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


ModifierSpeciesReference::ModifierSpeciesReference (SBMLNamespaces *sbmlns) :
  SimpleSpeciesReference(sbmlns)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}

/** @cond doxygen-libsbml-internal */

/* constructor for validators */
ModifierSpeciesReference::ModifierSpeciesReference() :
  SimpleSpeciesReference()
{
}

/** @endcond */
 
                          

/*
 * Destroys this ModifierSpeciesReference.
 */
ModifierSpeciesReference::~ModifierSpeciesReference ()
{
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * ModifierSpeciesReference (if available).
 */
bool
ModifierSpeciesReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return a (deep) copy of this ModifierSpeciesReference.
 */
ModifierSpeciesReference*
ModifierSpeciesReference::clone () const
{
  return new ModifierSpeciesReference(*this);
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
ModifierSpeciesReference::getTypeCode () const
{
  return SBML_MODIFIER_SPECIES_REFERENCE;
}


/*
 * @return the name of this element ie "modifierSpeciesReference".
 
 */
const string&
ModifierSpeciesReference::getElementName () const
{
  static const string name = "modifierSpeciesReference";
  return name;
}


bool 
ModifierSpeciesReference::hasRequiredAttributes() const
{
  bool allPresent = SimpleSpeciesReference::hasRequiredAttributes();

  return allPresent;
}


#ifdef USE_LAYOUT

/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
ModifierSpeciesReference::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  // This has to do additional work for reading annotations, so the code
  // here is copied and expanded from SBase::readNotes().

  if (name == "annotation")
  {
//    XMLNode* new_annotation = NULL;
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation != NULL)
    {
      if (getLevel() < 3) 
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
	        "Only one <annotation> element is permitted inside a "
	        "particular containing element.");
      }
      else
      {
        logError(MultipleAnnotations, getLevel(), getVersion());
      }
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    if (mCVTerms != NULL)
    {
      unsigned int size = mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
      delete mCVTerms;
    }
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
//    new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
//    delete mAnnotation;
//    mAnnotation = new_annotation;
    
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      parseSpeciesReferenceAnnotation(mAnnotation,*this);
//      new_annotation=deleteLayoutIdAnnotation(mAnnotation);
//      delete mAnnotation;
//      mAnnotation = new_annotation;
    }
    read=true;
  }

  return read;
}

/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
ModifierSpeciesReference::writeElements (XMLOutputStream& stream) const
{
  if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
  {
    if (this->isSetId())
    {
      ModifierSpeciesReference * sr = 
        const_cast <ModifierSpeciesReference *> (this);
      XMLNode * idAnnotation = parseLayoutId(this);
      if(mAnnotation == NULL)
      {
        if (idAnnotation != NULL) 
          static_cast <SBase *> (sr)->setAnnotation(idAnnotation);
      }
      else
      {
        if (idAnnotation != NULL) 
          static_cast <SBase *> (sr)->appendAnnotation(idAnnotation);
      }
    }
  }
  SBase::writeElements(stream);    
  
}

#endif // USE_LAYOUT


/*
 * Creates a new ListOfSpeciesReferences.
 */
ListOfSpeciesReferences::ListOfSpeciesReferences () : mType(Unknown)
{
}


/*
 * @return a (deep) copy of this ListOfUnits.
 */
ListOfSpeciesReferences*
ListOfSpeciesReferences::clone () const
{
  return new ListOfSpeciesReferences(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfSpeciesReferences::getItemTypeCode () const
{
  if (mType == Reactant || mType == Product)
  {
    return SBML_SPECIES_REFERENCE;
  }
  else if (mType == Modifier)
  {
    return SBML_MODIFIER_SPECIES_REFERENCE;
  }
  else
  {
    return SBML_UNKNOWN;
  }
}


/*
 * @return the name of this element ie "listOfReactants" or "listOfProducts" etc.
 */
const string&
ListOfSpeciesReferences::getElementName () const
{
  static const string unknown   = "listOfUnknowns";
  static const string reactants = "listOfReactants";
  static const string products  = "listOfProducts";
  static const string modifiers = "listOfModifiers";

       if (mType == Reactant) return reactants;
  else if (mType == Product ) return products;
  else if (mType == Modifier) return modifiers;
  else return unknown;
}


/* return nth item in list */
SimpleSpeciesReference *
ListOfSpeciesReferences::get(unsigned int n)
{
  return static_cast<SimpleSpeciesReference*>(ListOf::get(n));
}


/* return nth item in list */
const SimpleSpeciesReference *
ListOfSpeciesReferences::get(unsigned int n) const
{
  return static_cast<const SimpleSpeciesReference*>(ListOf::get(n));
}


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqSR : public unary_function<SBase*, bool>
{
  const string& id;

  IdEqSR (const string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return (static_cast <SimpleSpeciesReference *> (sb)->getId() == id)
       || (static_cast <SimpleSpeciesReference *> (sb)->getSpecies() == id); }
};


/* return item by id */
SimpleSpeciesReference*
ListOfSpeciesReferences::get (const std::string& sid)
{
  return const_cast<SimpleSpeciesReference*>( 
    static_cast<const ListOfSpeciesReferences&>(*this).get(sid) );
}


/* return item by id */
const SimpleSpeciesReference*
ListOfSpeciesReferences::get (const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  if (&(sid) == NULL)
  {
    return NULL;
  }
  else
  {
    result = find_if( mItems.begin(), mItems.end(), IdEqSR(sid) );
    return (result == mItems.end()) ? NULL : 
                             static_cast <SimpleSpeciesReference*> (*result);
  }
}


/* Removes the nth item from this list */
SimpleSpeciesReference*
ListOfSpeciesReferences::remove (unsigned int n)
{
   return static_cast<SimpleSpeciesReference*>(ListOf::remove(n));
}


/* Removes item in this list by id */
SimpleSpeciesReference*
ListOfSpeciesReferences::remove (const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  if (&(sid) != NULL)
  {
    result = find_if( mItems.begin(), mItems.end(), IdEqSR(sid) );

    if (result != mItems.end())
    {
      item = *result;
      mItems.erase(result);
    }
  }

  return static_cast <SimpleSpeciesReference*> (item);
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfSpeciesReferences::getElementPosition () const
{
  int position;

  switch (mType)
  {
    case Reactant: position =  1; break;
    case Product:  position =  2; break;
    case Modifier: position =  3; break;
    default:       position = -1; break;
  }

  return position;
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Sets type of this ListOfSpeciesReferences.
 */
void
ListOfSpeciesReferences::setType (SpeciesType type)
{
  mType = type;
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSpeciesReferences::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = NULL;


  if (mType == Reactant || mType == Product)
  {
    if (name == "speciesReference" || name == "specieReference")
    {
      try
      {
        object = new SpeciesReference(getSBMLNamespaces());
      }
      catch (SBMLConstructorException*)
      {
        object = new SpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
      catch ( ... )
      {
        object = new SpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
    }
    else if (name == "annotation" || name == "notes")
    {
      // do nothing
    }
    else
    {
      /* create the object anyway - or will also get unrecognized element message 
       * which is confusion if user has merely reversed modifierSpeciesReference
       * and speciesReference */
      try
      {
        object = new SpeciesReference(getSBMLNamespaces());
      }
      catch (SBMLConstructorException*)
      {
        object = new SpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
      catch ( ... )
      {
        object = new SpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
      logError(InvalidReactantsProductsList);
    }
  }
  else if (mType == Modifier)
  {
    if (name == "modifierSpeciesReference")
    {
      try
      {
        object = new ModifierSpeciesReference(getSBMLNamespaces());
      }
      catch (SBMLConstructorException*)
      {
        object = new ModifierSpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
      catch ( ... )
      {
        object = new ModifierSpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
    }
    else if (name == "annotation" || name == "notes")
    {
      // do nothing
    }
    else
    {
      try
      {
        object = new ModifierSpeciesReference(getSBMLNamespaces());
      }
      catch (SBMLConstructorException*)
      {
        object = new ModifierSpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
      catch ( ... )
      {
        object = new ModifierSpeciesReference(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
      }
      logError(InvalidModifiersList);
    }
  }

  if (object != NULL) mItems.push_back(object);

  return object;
}
/** @endcond */


/** @cond doxygen-c-only */

/**
 * Creates a new SpeciesReference_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesReference
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesReference
 *
 * @return a pointer to the newly created SpeciesReference_t structure.
 *
 * @note Once a SpeciesReference has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the SpeciesReference.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_create (unsigned int level, unsigned int version)
{
  try
  {
    SpeciesReference* obj = new SpeciesReference(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new SpeciesReference_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this SpeciesReference
 *
 * @return a pointer to the newly created SpeciesReference_t structure.
 *
 * @note Once a SpeciesReference has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the SpeciesReference.  Despite this, the ability to supply the values at 
 * creation time is an important aid to creating valid SBML.  Knowledge of the 
 * intended SBML Level and Version determine whether it is valid to assign a 
 * particular value to an attribute, or whether it is valid to add an object 
 * to an existing SBMLDocument.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    SpeciesReference* obj = new SpeciesReference(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new ModifierSpeciesReference (SpeciesReference_t) structure 
 * using the given SBMLNamespaces_t structure.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesReference
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesReference
 *
 * @return a pointer to the newly created SpeciesReference_t structure.
 *
 * @note Once a ModifierSpeciesReference has been added to an SBMLDocument, 
 * the @p level and @p version for the document @em override those used to 
 * create the ModifierSpeciesReference.  Despite this, the ability to supply 
 * the values at creation time is an important aid to creating valid SBML.  
 * Knowledge of the intended SBML Level and Version determine whether it is
 * valid to assign a particular value to an attribute, or whether it is valid 
 * to add an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createModifier (unsigned int level, unsigned int version)
{
  try
  {
    ModifierSpeciesReference* obj = new ModifierSpeciesReference(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new ModifierSpeciesReference (SpeciesReference_t) structure 
 * using the given SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this ModifierSpeciesReference
 *
 * @return a pointer to the newly created SpeciesReference_t structure.
 *
 * @note Once a ModifierSpeciesReference has been added to an SBMLDocument, 
 * the @p sbmlns namespaces for the document @em override those used to create
 * the ModifierSpeciesReference. Despite this, the ability to supply the values 
 * at creation time is an important aid to creating valid SBML.  Knowledge of 
 * the intended SBML Level and Version determine whether it is valid to assign a 
 * particular value to an attribute, or whether it is valid to add an object to 
 * an existing SBMLDocument.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createModifierWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    ModifierSpeciesReference* obj = new ModifierSpeciesReference(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Frees the given SpeciesReference_t structure.
 *
 * @param sr The SpeciesReference_t structure.
 */
LIBSBML_EXTERN
void
SpeciesReference_free (SpeciesReference_t *sr)
{
  delete sr;
}


/**
 * Creates and returns a deep copy of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return a (deep) copy of this SpeciesReference_t.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_clone (const SpeciesReference_t *sr)
{
  return (sr != NULL) ? static_cast<SpeciesReference_t*>( sr->clone() ) : NULL;
}


/**
 * Initializes the attributes of the given SpeciesReference_t structure to
 * their defaults:
 *
 * @li stoichiometry is set to @c 1
 * @li denominator is set to @c 1
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 */
LIBSBML_EXTERN
void
SpeciesReference_initDefaults (SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return;
    static_cast<SpeciesReference*>(sr)->initDefaults();
  }
}


/**
 * Returns a list of XMLNamespaces_t associated with this SpeciesReference_t
 * structure.
 *
 * @param sr the SpeciesReference_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
SpeciesReference_getNamespaces(SpeciesReference_t *sr)
{
  return (sr != NULL) ? sr->getNamespaces() : NULL;
}

/**
 * Predicate returning @c true or @c false depending on whether the
 * given SpeciesReference_t structure is a modifier.
 * 
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if this SpeciesReference_t represents a modifier
 * species, zero (0)if it is a plain SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference_isModifier (const SpeciesReference_t *sr)
{
  return (sr != NULL) ? static_cast<int>( sr->isModifier() ) : 0;
}


/**
 * Get the value of the "id" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the identifier of the SpeciesReference_t instance.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getId (const SpeciesReference_t *sr)
{
  return (sr != NULL && sr->isSetId()) ? sr->getId().c_str() : NULL;
}


/**
 * Get the value of the "name" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the name of the SpeciesReference_t instance.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getName (const SpeciesReference_t *sr)
{
  return (sr != NULL && sr->isSetName()) ? sr->getName().c_str() : NULL;
}


/**
 * Get the value of the "species" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the "species" attribute value
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getSpecies (const SpeciesReference_t *sr)
{
  return (sr != NULL && sr->isSetSpecies()) ? sr->getSpecies().c_str() : NULL;
}


/**
 * Get the value of the "stoichiometry" attribute of the given
 * SpeciesReference_t structure.
 *
 * This function returns zero if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the "stoichiometry" attribute value
 */
LIBSBML_EXTERN
double
SpeciesReference_getStoichiometry (const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return 0.0;
    return static_cast<const SpeciesReference*>(sr)->getStoichiometry();
  }
  else
    return numeric_limits<double>::quiet_NaN();
}


/**
 * Get the content of the "stoichiometryMath" subelement of the given
 * SpeciesReference_t structure.
 *
 * This function returns NULL if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the stoichiometryMath of this SpeciesReference.
 */
LIBSBML_EXTERN
StoichiometryMath_t *
SpeciesReference_getStoichiometryMath (SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return NULL;
    return static_cast<SpeciesReference*>(sr)->getStoichiometryMath();
  }
  else
    return NULL;
}


/**
 * Get the value of the "denominator" attribute, for the case of a
 * rational-numbered stoichiometry or a model in SBML Level 1.
 *
 * The "denominator" attribute is only actually written out in the case of
 * an SBML Level 1 model.  In SBML Level 2, rational-number stoichiometries
 * are written as MathML elements in the "stoichiometryMath" subelement.
 * However, as a convenience to users, libSBML allows the creation and
 * manipulation of rational-number stoichiometries by supplying the
 * numerator and denominator directly rather than having to manually create
 * an ASTNode structure.  LibSBML will write out the appropriate constructs
 * (either a combination of "stoichiometry" and "denominator" in the case
 * of SBML Level 1, or a "stoichiometryMath" subelement in the case of SBML
 * Level 2).
 *
 * This function returns 0 if the SpeciesReference_t structure is a Modifer (see
 * SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the denominator of this SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference_getDenominator (const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return 0;
    return static_cast<const SpeciesReference*>(sr)->getDenominator();
  }
  else
    return SBML_INT_MAX;
}


/**
 * Get the value of the "constant" attribute.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the constant attribute of this SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference_getConstant (const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return 0;
    return static_cast<const SpeciesReference*>(sr)->getConstant();
  }
  else
    return 0;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "id" attribute of the given SpeciesReference_t structure is
 * set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "id" attribute of given SpeciesReference_t
 * structure is set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetId (const SpeciesReference_t *sr)
{
  return (sr != NULL) ? static_cast<int>( sr->isSetId() ) : 0;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "name" attribute of the given SpeciesReference_t
 * structure is set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "name" attribute of given SpeciesReference_t
 * structure is set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetName (const SpeciesReference_t *sr)
{
  return (sr != NULL) ? static_cast<int>( sr->isSetName() ) : 0;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "species" attribute of the given SpeciesReference_t
 * structure is set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "species" attribute of given SpeciesReference_t
 * structure is set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetSpecies (const SpeciesReference_t *sr)
{
  return (sr != NULL) ? static_cast<int>( sr->isSetSpecies() ) : 0;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "stoichiometryMath" subelement of the given
 * SpeciesReference_t structure is non-empty.
 *
 * This function returns false if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "stoichiometryMath" subelement has content, zero
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetStoichiometryMath (const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return 0;

    return static_cast<int>
    (
      static_cast<const SpeciesReference*>(sr)->isSetStoichiometryMath()
    );
  }
  else
    return 0;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "stoichiometry" attribute of the given SpeciesReference_t structure is
 * set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "stoichiometry" attribute of given SpeciesReference_t
 * structure is set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetStoichiometry (const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return 0;

    return static_cast<int>( 
      static_cast<const SpeciesReference*>(sr)->isSetStoichiometry() );
  }
  else
    return 0;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "constant" attribute of the given SpeciesReference_t structure is
 * set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "constant" attribute of given SpeciesReference_t
 * structure is set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetConstant (const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return 0;

    return static_cast<int>
      (static_cast<const SpeciesReference*>(sr)->isSetConstant() );
  }
  else
    return 0;
}


/**
 * Sets the value of the "id" attribute of the given SpeciesReference_t
 * structure.
 *
 * The string in @p sid will be copied.
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param sid The identifier string that will be copied and assigned as the
 * "id" attribute value.
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
 * unsetting the "id" attribute.
 */
LIBSBML_EXTERN
int
SpeciesReference_setId (SpeciesReference_t *sr, const char *sid)
{
  if (sr != NULL)
    return (sid == NULL) ? sr->unsetId() : sr->setId(sid);
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
 * Sets the value of the "name" attribute of the given SpeciesReference_t
 * structure.
 *
 * The string in @p sid will be copied.
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param sid The identifier string that will be copied and assigned as the
 * "name" attribute value.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "name" attribute.
 */
LIBSBML_EXTERN
int
SpeciesReference_setName (SpeciesReference_t *sr, const char *name)
{
  if (sr != NULL)
    return (name == NULL) ? sr->unsetName() : sr->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
 * Sets the value of the "species" attribute of the given SpeciesReference_t
 * structure.
 *
 * The string in @p sid will be copied.
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param sid The identifier string that will be copied and assigned as the
 * "species" attribute value.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "species" attribute.
 */
LIBSBML_EXTERN
int
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sid)
{
  if (sr != NULL)
    return sr->setSpecies(sid != NULL ? sid : "");
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
 * Sets the value of the "stoichiometry" attribute of the given
 * SpeciesReference_t structure.
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param value The value to assign to the "stoichiometry" attribute.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
SpeciesReference_setStoichiometry (SpeciesReference_t *sr, double value)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return LIBSBML_UNEXPECTED_ATTRIBUTE;
    return static_cast<SpeciesReference*>(sr)->setStoichiometry(value);
  }
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
StoichiometryMath_t *
SpeciesReference_createStoichiometryMath (SpeciesReference_t *sr)
{
  return (sr != NULL) ? 
    static_cast<SpeciesReference*> (sr)->createStoichiometryMath() : NULL;
}

/**
 * Sets the content of the "stoichiometryMath" subelement of the given
 * SpeciesReference_t structure.
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param math An ASTNode expression tree to use as the content of the
 * "stoichiometryMath" subelement.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 */
LIBSBML_EXTERN
int
SpeciesReference_setStoichiometryMath (  SpeciesReference_t *sr
                                       , const StoichiometryMath_t    *math )
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return LIBSBML_UNEXPECTED_ATTRIBUTE;
    return static_cast<SpeciesReference*>(sr)->setStoichiometryMath(math);
  }
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
 * Sets the value of the "denominator" attribute of the given
 * SpeciesReference_t structure.
 *
 * The "denominator" attribute is only actually written out in the case of
 * an SBML Level 1 model.  In SBML Level 2, rational-number stoichiometries
 * are written as MathML elements in the "stoichiometryMath" subelement.
 * However, as a convenience to users, libSBML allows the creation and
 * manipulation of rational-number stoichiometries by supplying the
 * numerator and denominator directly rather than having to manually create
 * an ASTNode structure.  LibSBML will write out the appropriate constructs
 * (either a combination of "stoichiometry" and "denominator" in the case
 * of SBML Level 1, or a "stoichiometryMath" subelement in the case of SBML
 * Level 2).
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param value The value to assign to the "denominator" attribute.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
SpeciesReference_setDenominator (SpeciesReference_t *sr, int value)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return LIBSBML_UNEXPECTED_ATTRIBUTE;
    return static_cast<SpeciesReference*>(sr)->setDenominator(value);
  }
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
 * Assign the "constant" attribute of a SpeciesReference_t structure.
 *
 * @param p the SpeciesReference_t structure to set.
 * @param value the value to assign as the "constant" attribute
 * of the speciesReference, either zero for false or nonzero for true.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 */
LIBSBML_EXTERN
int
SpeciesReference_setConstant (SpeciesReference_t *sr, int value)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return LIBSBML_UNEXPECTED_ATTRIBUTE;
    return static_cast<SpeciesReference*>(sr)->setConstant(value);
  }
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
 * Unsets the value of the "id" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
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
SpeciesReference_unsetId (SpeciesReference_t *sr)
{
  return (sr != NULL) ? sr->unsetId() : LIBSBML_INVALID_OBJECT;
}


/**
 * Unsets the value of the "name" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
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
SpeciesReference_unsetName (SpeciesReference_t *sr)
{
  return (sr != NULL) ? sr->unsetName() : LIBSBML_INVALID_OBJECT;
}

/**
 * Unsets the content of the "stoichiometryMath" subelement of the given
 * SpeciesReference_t structure.
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
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
SpeciesReference_unsetStoichiometryMath (SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return LIBSBML_UNEXPECTED_ATTRIBUTE;
    return static_cast<SpeciesReference*>(sr)->unsetStoichiometryMath();
  }
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
 * Unsets the content of the "stoichiometry" attribute of the given
 * SpeciesReference_t structure.
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
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
SpeciesReference_unsetStoichiometry (SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    if (sr->isModifier()) return LIBSBML_UNEXPECTED_ATTRIBUTE;
    return static_cast<SpeciesReference*>(sr)->unsetStoichiometry();
  }
  else
    return LIBSBML_INVALID_OBJECT;
}


/**
  * Predicate returning @c true or @c false depending on whether
  * all the required attributes for this SpeciesReference object
  * have been set.
  *
 * @param p the SpeciesReference_t structure to check.
 *
  * @note The required attributes for a SpeciesReference object are:
  * @li species
  * @li constant (in L3 only)
  *
  * @return a true if all the required
  * attributes for this object have been defined, false otherwise.
  */
LIBSBML_EXTERN
int
SpeciesReference_hasRequiredAttributes(SpeciesReference_t *sr)
{
  return (sr != NULL) ? static_cast<int>(
    static_cast<SpeciesReference*>(sr)->hasRequiredAttributes()) : 0;
}


/**
 * @return item in this ListOfSpeciesReference with the given id or NULL if no such
 * item exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
ListOfSpeciesReferences_getById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfSpeciesReferences *> (lo)->get(sid) : NULL;
  else
    return NULL;
}


/**
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is responsible for
 * deleting it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
ListOfSpeciesReferences_removeById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfSpeciesReferences *> (lo)->remove(sid) : NULL;
  else
    return NULL;
}


/** @endcond */

LIBSBML_CPP_NAMESPACE_END
