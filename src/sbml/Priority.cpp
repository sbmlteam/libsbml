/**
 * @file    Priority.cpp
 * @brief   Implementation of Priority.
 * @author  Sarah Keating
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
#include <sbml/Priority.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

Priority::Priority (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mMath      ( NULL              )
 , mInternalId ( "" )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


Priority::Priority (SBMLNamespaces * sbmlns) :
   SBase ( sbmlns )
 , mMath      ( NULL              )
 , mInternalId ( "" )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
Priority::Priority() :
  SBase()
{
}

/** @endcond */
                          

/*
 * Destroys this Priority.
 */
Priority::~Priority ()
{
  delete mMath;
}


/*
 * Copy constructor. Creates a copy of this Priority.
 */
Priority::Priority (const Priority& orig) :
   SBase          ( orig                 )
 , mMath          ( NULL                   )
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mInternalId = orig.mInternalId;

    if (orig.mMath != NULL) 
    {
      mMath = orig.mMath->deepCopy();
      mMath->setParentSBMLObject(this);
    }
  }
}


/*
 * Assignment operator
 */
Priority& Priority::operator=(const Priority& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    this->mInternalId = rhs.mInternalId;

    delete mMath;
    if (rhs.mMath != NULL) 
    {
      mMath = rhs.mMath->deepCopy();
      mMath->setParentSBMLObject(this);
    }
    else
    {
      mMath = NULL;
    }
  }

  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
Priority::accept (SBMLVisitor& v) const
{
  v.visit(*this);
  v.leave(*this);
  return true;
}


/*
 * @return a (deep) copy of this Priority.
 */
Priority*
Priority::clone () const
{
  return new Priority(*this);
}



/*
 * @return the math of this Priority.
 */
const ASTNode*
Priority::getMath () const
{
  return mMath;
}


/*
 * @return true if the math (or equivalently the formula) of this
 * Priority is set, false otherwise.
 */
bool
Priority::isSetMath () const
{
  return (mMath != NULL);
}


/*
 * Sets the math of this Priority to a copy of the given ASTNode.
 */
int
Priority::setMath (const ASTNode* math)
{
  if (mMath == math) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMath;
    mMath = (math != NULL) ? math->deepCopy() : NULL;
    if (mMath != NULL) mMath->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Priority::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
Priority::setParentSBMLObject (SBase* sb)
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
Priority::getTypeCode () const
{
  return SBML_PRIORITY;
}


/*
 * @return the name of this element ie "priority".
 */
const string&
Priority::getElementName () const
{
  static const string name = "priority";
  return name;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Priority::getElementPosition () const
{
  return 1;
}
/** @endcond */


bool 
Priority::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for delay: math */

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
Priority::readOtherXML (XMLInputStream& stream)
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

    if (mMath != NULL)
    {
      if (getLevel() < 3) 
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
	        "Only one <math> element is permitted inside a "
	        "particular containing element.");
      }
      else
      {
        logError(OneMathPerPriority, getLevel(), getVersion());
      }
    }
    /* check for MathML namespace 
     * this may be explicitly declared here
     * or implicitly declared on the whole document
     */
    const XMLToken elem = stream.peek();
    const std::string prefix = checkMathMLNamespace(elem);

    delete mMath;
    mMath = readMathML(stream, prefix);
    if (mMath != NULL) mMath->setParentSBMLObject(this);
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
Priority::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();

  switch (level)
  {
  case 1:
    logError(NotSchemaConformant, getLevel(), getVersion(),
	      "Priority is not a valid component for this level/version.");
    return;
    break;
  case 2:
    logError(NotSchemaConformant, getLevel(), getVersion(),
	      "Priority is not a valid component for this level/version.");
    return;
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
Priority::readL3Attributes (const XMLAttributes& attributes)
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
        logUnknownAttribute(name, level, version, "<priority>");
      }
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L3v1 ->)
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
Priority::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  /* invalid level/version */
  if (level < 3)
  {
    return;
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L3v1 ->)
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
Priority::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getLevel() > 2 && isSetMath() ) writeMathML(getMath(), stream);
}
/** @endcond */


/** @cond doxygen-c-only */


/**
 * Creates a new Priority_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Priority
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Priority
 *
 * @return a pointer to the newly created Priority_t structure.
 *
 * @note Once a Priority has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the Priority.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
Priority_t *
Priority_create (unsigned int level, unsigned int version)
{
  try
  {
    Priority* obj = new Priority(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new Priority_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this Priority
 *
 * @return a pointer to the newly created Priority_t structure.
 *
 * @note Once a Priority has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the Priority.  Despite this, the ability to supply the values at creation time
 * is an important aid to creating valid SBML.  Knowledge of the intended SBML
 * Level and Version determine whether it is valid to assign a particular value
 * to an attribute, or whether it is valid to add an object to an existing
 * SBMLDocument.
 */
LIBSBML_EXTERN
Priority_t *
Priority_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    Priority* obj = new Priority(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Frees the given Priority_t structure.
 *
 * @param t the Priority_t structure to free.
 */
LIBSBML_EXTERN
void
Priority_free (Priority_t *t)
{
  delete t;
}


/**
 * Creates and returns a deep copy of the given Priority_t structure.
 *
 * @param t the Priority_t structure to copy. 
 *
 * @return a (deep) copy of the given Priority_t structure @p t.
 */
LIBSBML_EXTERN
Priority_t *
Priority_clone (const Priority_t *p)
{
  return (p != NULL) ? p->clone() : 0;
}


/**
 * Returns a list of XMLNamespaces_t associated with this Priority_t
 * structure.
 *
 * @param d the Priority_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
Priority_getNamespaces(Priority_t *p)
{
  return (p != NULL) ? p->getNamespaces() : NULL;
}

/**
 * Get the mathematical formula for a Priority_t structure and return it as
 * as an ASTNode structure.
 *
 * @param t the Priority_t structure to query.
 * 
 * @return an ASTNode_t structure representing the expression tree.
 */
LIBSBML_EXTERN
const ASTNode_t *
Priority_getMath (const Priority_t *p)
{
  return (p != NULL) ? p->getMath() : NULL;
}


/**
 * Predicate to test whether the formula for the given Priority_t structure
 * is set.
 *
 * @param t the Priority_t structure to query
 *
 * @return @c true if the formula (meaning the @c math subelement) of
 * this Priority is set, @c false otherwise.
 */
LIBSBML_EXTERN
int
Priority_isSetMath (const Priority_t *p)
{
  return (p != NULL) ? static_cast<int>( p->isSetMath() ) : 0;
}


/**
 * Sets the math expression of the given Priority_t instance to a copy of the
 * given ASTNode_t structure.
 *
 * @param t the Priority_t structure to set.
 * @param math an ASTNode representing a formula tree.
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
Priority_setMath (Priority_t *p, const ASTNode_t *math)
{
  if (p != NULL)
    return p->setMath(math);
  else
    return LIBSBML_INVALID_OBJECT;
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END
