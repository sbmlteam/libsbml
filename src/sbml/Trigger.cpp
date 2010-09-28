/**
 * @file    Trigger.cpp
 * @brief   Implementation of Trigger.
 * @author  Sarah Keating
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
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Parameter.h>
#include <sbml/Trigger.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

Trigger::Trigger (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mMath      ( 0              )
 , mInitialValue      ( true )
 , mPersistent        ( true )
 , mIsSetInitialValue ( false )
 , mIsSetPersistent   ( false )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


Trigger::Trigger (SBMLNamespaces * sbmlns) :
   SBase ( sbmlns )
 , mMath      ( 0              )
 , mInitialValue      ( true )
 , mPersistent        ( true )
 , mIsSetInitialValue ( false )
 , mIsSetPersistent   ( false )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
Trigger::Trigger() :
  SBase()
{
}

/** @endcond */
                          

/*
 * Destroys this Trigger.
 */
Trigger::~Trigger ()
{
  delete mMath;
}


/*
 * Copy constructor. Creates a copy of this Trigger.
 */
Trigger::Trigger (const Trigger& orig) :
   SBase          ( orig )
 , mMath          ( 0    )
 , mInitialValue      ( orig.mInitialValue )
 , mPersistent        ( orig.mPersistent )
 , mIsSetInitialValue ( orig.mIsSetInitialValue )
 , mIsSetPersistent   ( orig.mIsSetPersistent )
{
  if (orig.mMath) 
  {
    mMath = orig.mMath->deepCopy();
    mMath->setParentSBMLObject(this);
  }
}


/*
 * Assignment operator.
 */
Trigger& Trigger::operator=(const Trigger& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    this->mInitialValue      = rhs.mInitialValue;
    this->mPersistent        = rhs.mPersistent;
    this->mIsSetInitialValue = rhs.mIsSetInitialValue;
    this->mIsSetPersistent   = rhs.mIsSetPersistent;

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
Trigger::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return a (deep) copy of this Trigger.
 */
Trigger*
Trigger::clone () const
{
  return new Trigger(*this);
}


/*
 * @return the math of this Trigger.
 */
const ASTNode*
Trigger::getMath () const
{
  return mMath;
}


/*
 * @return the initialValue of this Trigger.
 */
bool
Trigger::getInitialValue () const
{
  return mInitialValue;
}


/*
 * @return the persistent of this Trigger.
 */
bool
Trigger::getPersistent () const
{
  return mPersistent;
}


/*
 * @return true if the math (or equivalently the formula) of this
 * Trigger has been set, false otherwise.
 */
bool
Trigger::isSetMath () const
{
  return (mMath != 0);
}



/*
 * @return true if initialValue is set of this Trigger.
 */
bool
Trigger::isSetInitialValue () const
{
  return mIsSetInitialValue;
}


/*
 * @return true if persistent is set of this Trigger.
 */
bool
Trigger::isSetPersistent () const
{
  return mIsSetPersistent;
}


/*
 * Sets the math of this Trigger to a copy of the given ASTNode.
 */
int
Trigger::setMath (const ASTNode* math)
{
  if (mMath == math) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = 0;
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
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the initialvalue of this Trigger.
 */
int
Trigger::setInitialValue (bool initialValue)
{
  if (getLevel() < 3)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mInitialValue = initialValue;
    mIsSetInitialValue = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the persistent of this Trigger.
 */
int
Trigger::setPersistent (bool persistent)
{
  if (getLevel() < 3)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mPersistent = persistent;
    mIsSetPersistent = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Trigger::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
Trigger::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}
/** @endcond */


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Trigger::getTypeCode () const
{
  return SBML_TRIGGER;
}


/*
 * @return the name of this element ie "trigger".
 */
const string&
Trigger::getElementName () const
{
  static const string name = "trigger";
  return name;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Trigger::getElementPosition () const
{
  return 0;
}
/** @endcond */


bool 
Trigger::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for trigger: math */

  if (!isSetMath())
    allPresent = false;

  return allPresent;
}


bool 
Trigger::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for event: persistent and initialvalue (L3 ->) 
   */

  if (getLevel() > 2)
  {
    if(!isSetPersistent())
      allPresent = false;

    if(!isSetInitialValue())
      allPresent = false;
  }

  return allPresent;
}


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Trigger::readOtherXML (XMLInputStream& stream)
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

    if (mMath)
    {
      if (getLevel() < 3) 
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
	        "Only one <math> element is permitted inside a "
	        "particular containing element.");
      }
      else
      {
        logError(OneMathPerTrigger, getLevel(), getVersion());
      }
    }
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
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Trigger::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();

  switch (level)
  {
  case 1:
    logError(NotSchemaConformant, getLevel(), getVersion(),
	      "Trigger is not a valid component for this level/version.");
    return;
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
Trigger::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  if (version > 2)
  {
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
        logUnknownAttribute(name, level, version, "<trigger>");
      }
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3->)
  //
  if (version > 2)
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Trigger::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("sboTerm");
  expectedAttributes.push_back("initialValue");
  expectedAttributes.push_back("persistent");

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
        logUnknownAttribute(name, level, version, "<trigger>");
      }
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3->)
  //
  mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);

  //
  // initailValue { use="required"}  (L3v1 ->)
  //
  mIsSetInitialValue = attributes.readInto("initialValue", 
                        mInitialValue, getErrorLog());

  if (!mIsSetInitialValue)
  {
    logError(AllowedAttributesOnTrigger, level, version);
  }

  //
  // persistent { use="required"}  (L3v1 ->)
  //
  mIsSetPersistent = attributes.readInto("persistent", 
                        mPersistent, getErrorLog());

  if (!mIsSetPersistent)
  {
    logError(AllowedAttributesOnTrigger, level, version);
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
Trigger::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();
 
  /* invalid level/version */
  if (level < 2)
  {
    return;
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3->)
  //
  if (!(level == 2 && version < 3))
    SBO::writeTerm(stream, mSBOTerm);

  if (level > 2)
  {
    stream.writeAttribute("initialValue", mInitialValue);
    stream.writeAttribute("persistent", mPersistent);
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
Trigger::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getLevel() > 1 && isSetMath() ) writeMathML(getMath(), stream);
}
/** @endcond */



/** @cond doxygen-c-only */

/**
 * Creates a new Trigger_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Trigger
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Trigger
 *
 * @return a pointer to the newly created Trigger_t structure.
 *
 * @note Once a Trigger has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the Trigger.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_create (unsigned int level, unsigned int version)
{
  try
  {
    Trigger* obj = new Trigger(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new Trigger_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this Trigger
 *
 * @return a pointer to the newly created Trigger_t structure.
 *
 * @note Once a Trigger has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the Trigger.  Despite this, the ability to supply the values at creation time
 * is an important aid to creating valid SBML.  Knowledge of the intended SBML
 * Level and Version determine whether it is valid to assign a particular value
 * to an attribute, or whether it is valid to add an object to an existing
 * SBMLDocument.
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    Trigger* obj = new Trigger(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Frees the given Trigger.
 */
LIBSBML_EXTERN
void
Trigger_free (Trigger_t *t)
{
  delete t;
}


/**
 * @return a (deep) copy of this Trigger.
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_clone (const Trigger_t *t)
{
  return t->clone();
}


/**
 * Returns a list of XMLNamespaces_t associated with this Trigger_t
 * structure.
 *
 * @param t the Trigger_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
Trigger_getNamespaces(Trigger_t *t)
{
  return t->getNamespaces();
}

/**
 * @return the math of this Trigger.
 */
LIBSBML_EXTERN
const ASTNode_t *
Trigger_getMath (const Trigger_t *t)
{
  return t->getMath();
}


/**
 * Get the value of the "initialValue" attribute of this Trigger.
 * 
 * @param t the Trigger_t structure
 *
 * @return the "initialValue" attribute value
 * in this Trigger.
 */
LIBSBML_EXTERN
int
Trigger_getInitialValue (const Trigger_t *t)
{
  return static_cast<int>(t->getInitialValue());
}


/**
 * Get the value of the "persistent" attribute of this Trigger.
 * 
 * @param t the Trigger_t structure
 *
 * @return the "persistent" attribute value
 * in this Trigger.
 */
LIBSBML_EXTERN
int
Trigger_getPersistent (const Trigger_t *t)
{
  return static_cast<int>(t->getPersistent());
}


/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this Trigger has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetMath (const Trigger_t *t)
{
  return static_cast<int>( t->isSetMath() );
}


/**
 * Return true if the  "initialValue" attribute of this Trigger has been set.
 * 
 * @param t the Trigger_t structure
 *
 * @return true if the "initialValue" attribute value
 * in this Trigger has been set, false otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetInitialValue (const Trigger_t *t)
{
  return static_cast<int>( t->isSetInitialValue() );
}


/**
 * Return true if the  "persistent" attribute of this Trigger has been set.
 * 
 * @param t the Trigger_t structure
 *
 * @return true if the "persisent" attribute value
 * in this Trigger has been set, false otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetPersistent (const Trigger_t *t)
{
  return static_cast<int>( t->isSetPersistent() );
}


/**
 * Sets the math of this Trigger to a copy of the given ASTNode.
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
Trigger_setMath (Trigger_t *t, const ASTNode_t *math)
{
  return t->setMath(math);
}


/**
 * Sets the "initialValue" attribute of this Trigger instance.
 *
 * @param t the Trigger_t structure
 * @param initialValue a boolean representing the initialValue to be set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
 * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
 */
LIBSBML_EXTERN
int
Trigger_setInitialValue (Trigger_t *t, int initialValue)
{
  return t->setInitialValue( static_cast<bool>(initialValue) );
}


/**
 * Sets the "persistent" attribute of this Trigger instance.
 *
 * @param t the Trigger_t structure
 * @param persistent a boolean representing the initialValue to be set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
 * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
 */
LIBSBML_EXTERN
int
Trigger_setPersistent (Trigger_t *t, int persistent)
{
  return t->setPersistent( static_cast<bool>(persistent) );
}


/**
  * Predicate returning @c true or @c false depending on whether
  * all the required attributes for this Trigger object
  * have been set.
  *
  * @note The required attributes for a Trigger object are:
  * @li persistent ( L3 onwards )
  * @li initialValue ( L3 onwards )
  */
LIBSBML_EXTERN
int
Trigger_hasRequiredAttributes (Trigger_t *t)
{
  return static_cast <int> (t->hasRequiredAttributes());
}



/**
  * Predicate returning @c true or @c false depending on whether
  * all the required elements for this Trigger object
  * have been set.
  *
  * @note The required elements for a Trigger object are:
  * @li math
  */
LIBSBML_EXTERN
int
Trigger_hasRequiredElements (Trigger_t *t)
{
  return static_cast <int> (t->hasRequiredElements() );
}




/** @endcond */

LIBSBML_CPP_NAMESPACE_END
