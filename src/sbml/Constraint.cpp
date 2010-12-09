/**
 * @file    Constraint.cpp
 * @brief   Implementations of Constraint and ListOfConstraints.
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

#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/Constraint.h>

#include <sbml/util/util.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

Constraint::Constraint (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mMath   (  0 )
 , mMessage(  0 )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


Constraint::Constraint (SBMLNamespaces* sbmlns) :
   SBase   ( sbmlns )
 , mMath   (  0 )
 , mMessage(  0 )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
Constraint::Constraint() :
  SBase()
{
}

/** @endcond */
                          

/*
 * Destroys this Constraint.
 */
Constraint::~Constraint ()
{
  if(mMath)    delete mMath;
  if(mMessage) delete mMessage;
}


/*
 * Copy constructor. Creates a copy of this Constraint.
 */
Constraint::Constraint (const Constraint& orig) :
   SBase   ( orig )
 , mMath   ( 0   )
 , mMessage( 0   )
{
  if (orig.mMath)    
  {
    mMath    = orig.mMath->deepCopy();
    mMath->setParentSBMLObject(this);
  }
  if (orig.mMessage) mMessage = new XMLNode(*orig.mMessage);
}


/*
 * Assignment operator
 */
Constraint& Constraint::operator=(const Constraint& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);

    delete mMath;
    if (rhs.mMath)    
    {
      mMath    = rhs.mMath->deepCopy();
      mMath->setParentSBMLObject(this);
    }
    else
    {
      mMath = 0;
    }

    delete mMessage;
    if (rhs.mMessage) 
      mMessage = new XMLNode(*rhs.mMessage);
    else
      mMessage = 0;
  }

  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * Constraint (if available).
 */
bool
Constraint::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return a (deep) copy of this Constraint.
 */
Constraint*
Constraint::clone () const
{
  return new Constraint(*this);
}


/*
 * @return the message for this Constraint.
 */
const XMLNode*
Constraint::getMessage () const
{
  return mMessage;
}


/*
 * @return the message for this Constraint.
 */
std::string
Constraint::getMessageString () const
{
  return mMessage->toXMLString();
}


/*
 * @return the math for this Constraint.
 */
const ASTNode*
Constraint::getMath () const
{
  return mMath;
}


/*
 * @return true if the message of this Constraint has been set,
 * false otherwise.
 */
bool
Constraint::isSetMessage () const
{
  return (mMessage != 0);
}


/*
 * @return true if the math for this Constraint has been set,
 * false otherwise.
 */
bool
Constraint::isSetMath () const
{
  return (mMath != 0);
}


/*
 * Sets the message of this Constraint to a copy of xhtml.
 */
int
Constraint::setMessage (const XMLNode* xhtml)
{
  if (mMessage == xhtml)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (xhtml == NULL)
  {
    delete mMessage;
    mMessage = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!SyntaxChecker::hasExpectedXHTMLSyntax(xhtml, getSBMLNamespaces()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMessage;
    mMessage = (xhtml != 0) ? new XMLNode(*xhtml) : 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the math of this Constraint to a copy of the given
 * ASTNode.
 */
int
Constraint::setMath (const ASTNode* math)
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
 * Unsets the message of this Constraint.
 */
int 
Constraint::unsetMessage ()
{
  delete mMessage;
  mMessage = 0;
  
  if (mMessage)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Constraint::getTypeCode () const
{
  return SBML_CONSTRAINT;
}


/*
 * @return the name of this element ie "constraint".
 */
const string&
Constraint::getElementName () const
{
  static const string name = "constraint";
  return name;
}


bool 
Constraint::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for constraint: math */

  if (!isSetMath())
    allPresent = false;

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
Constraint::readOtherXML (XMLInputStream& stream)
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
        logError(OneMathElementPerConstraint, getLevel(), getVersion());
      }
    }
    // If there's a <message>, it's supposed to show up first

    if (mMessage && getLevel() == 2) logError(IncorrectOrderInConstraint);

    /* check for MathML namespace 
     * this may be explicitly declared here
     * or implicitly declared on the whole document
     */
    const XMLToken elem = stream.peek();
    const std::string prefix = checkMathMLNamespace(elem);

    delete mMath;
  
    mMath = readMathML(stream, prefix);
    if (mMath) mMath->setParentSBMLObject(this);
    read  = true;
  }
  else if (name == "message")
  {
    if (mMessage)
    {
      if (getLevel() < 3) 
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
	        "Only one <message> element is permitted inside a "
	        "particular containing element.");
      }
      else
      {
        logError(OneMessageElementPerConstraint, getLevel(), getVersion());
      }
    }
    delete mMessage;

    mMessage = new XMLNode(stream);

    //
    // checks if the given default namespace (if any) is a valid
    // SBML namespace
    //
    const XMLNamespaces &xmlns = mMessage->getNamespaces();
    checkDefaultNamespace(&xmlns,"message");

    if (getSBMLDocument() != NULL)
    {
      if (getSBMLDocument()->getNumErrors() == 0)
      {
        checkXHTML(mMessage);
      }
    }
    read     = true;
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
Constraint::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  switch (level)
  {
  case 1:
    logError(NotSchemaConformant, getLevel(), getVersion(),
	      "Constraint is not a valid component for this level/version.");
    break;
  case 2:
    readL2Attributes(attributes);
    break;
  case 3:
  default:
    readL3Attributes(attributes);
    break;
  }
}/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Constraint::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  if (version == 1)
  {
    logError(NotSchemaConformant, getLevel(), getVersion(),
	      "Constraint is not a valid component for this level/version.");
    return;
  }

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("sboTerm");

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
        logUnknownAttribute(name, level, version, "<constraint>");
      }
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 -> )
  //
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
Constraint::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("sboTerm");

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
        logUnknownAttribute(name, level, version, "<constraint>");
      }
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 -> )
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
Constraint::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  /* invalid level/version */
  if (level < 2 || (level == 2 && version == 1))
  {
    return;
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Constraint::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mMath) writeMathML(mMath, stream);

  if (mMessage) stream << *mMessage;
}
/** @endcond */


/*
 * @return a (deep) copy of this ListOfConstraints.
 */
ListOfConstraints*
ListOfConstraints::clone () const
{
  return new ListOfConstraints(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfConstraints::getItemTypeCode () const
{
  return SBML_CONSTRAINT;
}


/*
 * @return the name of this element ie "listOfConstraints".
 */
const string&
ListOfConstraints::getElementName () const
{
  static const string name = "listOfConstraints";
  return name;
}


/* return nth item in list */
Constraint *
ListOfConstraints::get(unsigned int n)
{
  return static_cast<Constraint*>(ListOf::get(n));
}


/* return nth item in list */
const Constraint *
ListOfConstraints::get(unsigned int n) const
{
  return static_cast<const Constraint*>(ListOf::get(n));
}


/* Removes the nth item from this list */
Constraint*
ListOfConstraints::remove (unsigned int n)
{
   return static_cast<Constraint*>(ListOf::remove(n));
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfConstraints::getElementPosition () const
{
  return 10;
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfConstraints::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "constraint")
  {
    try
    {
      object = new Constraint(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      object = new Constraint(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      object = new Constraint(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    
    if (object) mItems.push_back(object);
  }

  return object;
}
/** @endcond */



/** @cond doxygen-c-only */


/**
 * Creates a new Constraint_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Constraint
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Constraint
 *
 * @return a pointer to the newly created Constraint_t structure.
 *
 * @note Once a Constraint has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the Constraint.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_create (unsigned int level, unsigned int version)
{
  try
  {
    Constraint* obj = new Constraint(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new Constraint_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this Constraint
 *
 * @return a pointer to the newly created Constraint_t structure.
 *
 * @note Once a Constraint has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the Constraint.  Despite this, the ability to supply the values at creation 
 * time is an important aid to creating valid SBML.  Knowledge of the intended 
 * SBML Level and Version determine whether it is valid to assign a particular 
 * value to an attribute, or whether it is valid to add an object to an existing
 * SBMLDocument.
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    Constraint* obj = new Constraint(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Frees the given Constraint_t structure.
 */
LIBSBML_EXTERN
void
Constraint_free (Constraint_t *c)
{
  delete c;
}


/**
 * Creates and returns a deep copy of the given Constraint_t structure.
 *
 * @param c the Constraint_t structure to copy
 * 
 * @return a (deep) copy of Constraint_t.
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_clone (const Constraint_t *c)
{
  return static_cast<Constraint*>( c->clone() );
}


/**
 * Returns a list of XMLNamespaces_t associated with this Constraint_t
 * structure.
 *
 * @param c the Constraint_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
Constraint_getNamespaces(Constraint_t *c)
{
  return c->getNamespaces();
}


/**
 * Get the message, if any, associated with this Constraint
 *
 * @param c the Constraint_t structure 
 * 
 * @return the message for this Constraint, as an XMLNode.
 */
LIBSBML_EXTERN
const XMLNode_t *
Constraint_getMessage (const Constraint_t *c)
{
  return c->getMessage();
}


/**
 * Get the message string, if any, associated with this Constraint
 *
 * @param c the Constraint_t structure 
 * 
 * @return the message for this Constraint, as a string (char*).
 * NULL is returned if the message is not set.
 *
 * @notice returned char* should be freed with safe_free() by the caller.
 */
LIBSBML_EXTERN
char*
Constraint_getMessageString (const Constraint_t *c)
{
  return c->isSetMessage() ? safe_strdup(c->getMessageString().c_str()) : NULL;
}


/**
 * Get the mathematical expression of this Constraint
 *
 * @param c the Constraint_t structure 
 * 
 * @return the math for this Constraint, as an ASTNode.
 */
LIBSBML_EXTERN
const ASTNode_t *
Constraint_getMath (const Constraint_t *c)
{
  return c->getMath();
}


/**
 * Predicate returning @c true or @c false depending on whether a
 * message has been defined for this Constraint.
 *
 * @param c the Constraint_t structure 
 * 
 * @return a nonzero integer if the "message" subelement for this
 * Constraint has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Constraint_isSetMessage (const Constraint_t *c)
{
  return static_cast<int>( c->isSetMessage() );
}


/**
 * Predicate returning @c true or @c false depending on whether a
 * mathematical formula has been defined for this Constraint.
 *
 * @param c the Constraint_t structure 
 * 
 * @return a nonzero integer if the "math" subelement for this Constraint
 * has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Constraint_isSetMath (const Constraint_t *c)
{
  return static_cast<int>( c->isSetMath() );
}


/**
 * Sets the message of this Constraint.
 *
 * @param c the Constraint_t structure
 *
 * @param xhtml an XML tree containing XHTML content.
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
Constraint_setMessage (Constraint_t *c, const XMLNode_t *xhtml)
{
  return c->setMessage(xhtml);
}


/**
 * Sets the mathematical expression of this Constraint.
 *
 * @param c the Constraint_t structure
 *
 * @param math an ASTNode expression to be assigned as the "math"
 * subelement of this Constraint
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
Constraint_setMath (Constraint_t *c, const ASTNode_t *math)
{
  return c->setMath(math);
}


/**
 * Unsets the "message" subelement of this Constraint.
 *
 * @param c the Constraint_t structure
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
Constraint_unsetMessage (Constraint_t *c)
{
  return c->unsetMessage();
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END
