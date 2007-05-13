/**
 * @file    FunctionDefinition.cpp
 * @brief   Implementation of FunctionDefinition and ListOfFunctionDefinitions.
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

#include <cstring>

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/FunctionDefinition.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new FunctionDefinition, optionally with its id and math (via
 * an infix formula string) attributes set.
 */
FunctionDefinition::FunctionDefinition (  const string& id
                                        , const string& formula ) :
    SBase( id, "", -1 )
  , mMath( SBML_parseFormula( formula.c_str() ) )
{
}


/**
 * Creates a new FunctionDefinition, optionally with its id and math
 * attributes set.
 */
FunctionDefinition::FunctionDefinition (  const string&  id
                                        , const ASTNode* math ) :
   SBase( id )
 , mMath( 0  )
{
  if (math) mMath = math->deepCopy();
}


/**
 * Destroys this FunctionDefinition.
 */
FunctionDefinition::~FunctionDefinition ()
{
  delete mMath;
}


/**
 * Copy constructor. Creates a copy of this FunctionDefinition.
 */
FunctionDefinition::FunctionDefinition (const FunctionDefinition& orig) :
   SBase( orig )
 , mMath( 0    )
{
  if (orig.mMath) mMath = orig.mMath->deepCopy();
}


/**
 * Assignment operator
 */
FunctionDefinition& FunctionDefinition::operator=(const FunctionDefinition& rhs)
{
  this->SBase::operator =(rhs);
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * FunctionDefinition (if available).
 */
bool
FunctionDefinition::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this FunctionDefinition.
 */
SBase*
FunctionDefinition::clone () const
{
  return new FunctionDefinition(*this);
}


/**
 * @return the math of this FunctionDefinition.
 */
const ASTNode*
FunctionDefinition::getMath () const
{
  return mMath;
}


/**
 * @return true if the math of this FunctionDefinition has been set, false
 * otherwise.
 */
bool
FunctionDefinition::isSetMath () const
{
  return (mMath != 0);
}

/**
 * Sets the math of this FunctionDefinition to the given ASTNode.
 */
void
FunctionDefinition::setMath (const ASTNode* math)
{
  if (mMath == math) return;

  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;
}

/**
 * @return the nth argument (bound variable) passed to this
 * FunctionDefinition.
 */
const ASTNode*
FunctionDefinition::getArgument (unsigned int n) const
{
  return (n < getNumArguments()) ? mMath->getChild(n) : 0;
}


/**
 * @return the argument (bound variable) in this FunctionDefinition with
 * the given name or NULL if no such argument exists.
 */
const ASTNode*
FunctionDefinition::getArgument (const string& name) const
{
  const char*    cname = name.c_str();
  const ASTNode* found = 0;


  for (unsigned int n = 0; n < getNumArguments(); ++n)
  {
    const ASTNode* node = getArgument(n);

    if (node && node->isName() && !strcmp(node->getName(), cname))
    {
      found = node;
      break;
    }
  }

  return found;
}


/**
 * @return the body of this FunctionDefinition, or NULL if no body is
 * defined.
 */
const ASTNode*
FunctionDefinition::getBody () const
{
  return mMath->getRightChild();
}


/**
 * @return the body of this FunctionDefinition, or NULL if no body is
 * defined.
 */
ASTNode*
FunctionDefinition::getBody ()
{
  return mMath->getRightChild();
}


/**
 * @return the number of arguments (bound variables) that must be passed
 * to this FunctionDefinition.
 */
unsigned int
FunctionDefinition::getNumArguments () const
{
  if (!isSetMath() || mMath->getNumChildren() == 0) return 0;
  else return mMath->getNumChildren() - 1;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
FunctionDefinition::getTypeCode () const
{
  return SBML_FUNCTION_DEFINITION;
}


/**
 * @return the name of this element ie "functionDefinition".
 */
const string&
FunctionDefinition::getElementName () const
{
  static const string name = "functionDefinition";
  return name;
}


/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
FunctionDefinition::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();


  if (name == "math")
  {
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
      mSBML->getErrorLog()->logError(10201);
    }

    delete mMath;
    mMath = readMathML(stream);
    read  = true;
  }
  else if (name == "annotation")
  {
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
    read = true;
  }
  else if (name == "notes")
  {
    /* if notes already exists then it is an error 
     * if annotation already exists then ordering is wrong
     */
    if (mNotes || mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }

    delete mNotes;
    mNotes = new XMLNode(stream);
    checkXHTML(mNotes);
    read = true;
  }

  return read;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
FunctionDefinition::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // id: SId  { use="required" }  (L2v1, L2v2)
  //
  attributes.readInto("id", mId);
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  attributes.readInto("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());

}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
FunctionDefinition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // id: SId  { use="required" }  (L2v1, L2v2)
  //
  stream.writeAttribute("id", mId);

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  stream.writeAttribute("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBO::writeTerm(stream, mSBOTerm);

}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
FunctionDefinition::writeElements (XMLOutputStream& stream) const
{
  if (mMath) writeMathML(mMath, stream);
}



/**
 * @return a (deep) copy of this ListOfFunctionDefinitions.
 */
SBase*
ListOfFunctionDefinitions::clone () const
{
  return new ListOfFunctionDefinitions(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfFunctionDefinitions::getItemTypeCode () const
{
  return SBML_FUNCTION_DEFINITION;
}


/**
 * @return the name of this element ie "listOfFunctionDefinitions".
 */
const string&
ListOfFunctionDefinitions::getElementName () const
{
  static const string name = "listOfFunctionDefinitions";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfFunctionDefinitions::getElementPosition () const
{
  return 1;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfFunctionDefinitions::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "functionDefinition")
  {
    object = new FunctionDefinition();
    mItems.push_back(object);
  }

  return object;
}


/** @cond doxygen-c-only */



/**
 * Creates a new, empty FunctionDefinition_t structure and returns a
 * pointer to it.
 * 
 * @return a pointer to the freshly-created FunctionDefinition_t structure
 *
 * @note It is worth emphasizing that although no identifier is supplied in
 * this constructor and no default identifier is provided, the "id"
 * (identifier) attribute of a FunctionDefinition_t structure is required
 * to have a value.  Thus, callers are cautioned to assign a value after
 * calling this constructor using FunctionDefinition_setId().
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_create (void)
{
  return new(nothrow) FunctionDefinition("", "");
}


/**
 * Creates a new FunctionDefinition_t structure with the given identifier
 * and mathematical formula.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   fd = FunctionDefinition_create();
 *   FunctionDefinition_setId(fd, id);
 *   FunctionDefinition_setMath(fd, math);
 * @endcode
 *
 * @param sid the identifier to be assigned to the FunctionDefinition_t
 * structure
 *
 * @param math an ASTNode_t tree structure defining the mathematical
 * formula implemented by this function
 *
 * @return a pointer to the new FunctionDefinition_t structure.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_createWith (const char *sid, ASTNode_t *math)
{
  return new(nothrow) FunctionDefinition(sid ? sid : "", math);
}


/**
 * Frees the given FunctionDefinition.
 *
 * @param fd the FunctionDefinition_t structure
 */
LIBSBML_EXTERN
void
FunctionDefinition_free (FunctionDefinition_t *fd)
{
  delete fd;
}


/**
 * Get the identifier of the given FunctionDefinition_t structure.
 *
 * @param fd the FunctionDefinition_t structure
 * 
 * @return the value of the "id" attribute of this FunctionDefinition_t
 * structure.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getId (const FunctionDefinition_t *fd)
{
  return fd->isSetId() ? fd->getId().c_str() : NULL;
}


/**
 * Get the name of the given FunctionDefinition_t structure.
 *
 * @param fd the FunctionDefinition_t structure
 *
 * @return the name of this FunctionDefinition_t structure.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getName (const FunctionDefinition_t *fd)
{
  return fd->isSetName() ? fd->getName().c_str() : NULL;
}


/**
 * Get the mathematical formula implemented by the given function.
 *
 * @param fd the FunctionDefinition_t structure
 * 
 * @return an ASTNode_t tree representing the mathematical formula of this
 * FunctionDefinition_t structure.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getMath (const FunctionDefinition_t *fd)
{
  return fd->getMath();
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Compartment_t structure's "id" attribute has been set.
 *
 * @param fd the FunctionDefinition_t structure
 *
 * @return nonzero if the "id" attribute of the FunctionDefinition_t
 * structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetId (const FunctionDefinition_t *fd)
{
  return static_cast<int>( fd->isSetId() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Compartment_t structure's "name" attribute has been set.
 *
 * @param fd the FunctionDefinition_t structure
 *
 * @return nonzero if the "name" attribute of the FunctionDefinition_t
 * structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetName (const FunctionDefinition_t *fd)
{
  return static_cast<int>( fd->isSetName() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Compartment_t structure's "math" subelement has been set.
 *
 * @param fd the FunctionDefinition_t structure
 *
 * @return nonzero if the mathematical expression of this
 * FunctionDefinition has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetMath (const FunctionDefinition_t *fd)
{
  return static_cast<int>( fd->isSetMath() );
}


/**
 * Sets the value of the "id" attribute of a FunctionDefinition_t structure.
 *
 * @param fd the FunctionDefinition_t structure to set.
 *
 * @param sid the identifier to assign to the "id" attribute of @p fd
 */
LIBSBML_EXTERN
void
FunctionDefinition_setId (FunctionDefinition_t *fd, const char *sid)
{
  fd->setId(sid ? sid : "");
}


/**
 * Sets the value of the "name" attribute of a FunctionDefinition_t structure.
 *
 * @param fd the FunctionDefinition_t structure to set.
 *
 * @param name the identifier to assign to the "name" attribute of @p fd
 */
LIBSBML_EXTERN
void
FunctionDefinition_setName (FunctionDefinition_t *fd, const char *name)
{
  (name == NULL) ? fd->unsetName() : fd->setName(name);
}


/**
 * Sets the "math" subelement of a given FunctionDefinition_t structure.
 *
 * The ASTNode_t structure given in @p math is copied.
 *
 * @param fd the FunctionDefinition_t structure to set.
 *
 * @param math the ASTNode_t structure to copy and assign to the "math"
 * subelement of the given FunctionDefinition_t structure
 */
LIBSBML_EXTERN
void
FunctionDefinition_setMath (FunctionDefinition_t *fd, const ASTNode_t *math)
{
  fd->setMath(math);
}


/**
 * Unsets the "name" attribute of the given FunctionDefinition_t structure.
 * 
 * @param fd the FunctionDefinition_t structure
 */
LIBSBML_EXTERN
void
FunctionDefinition_unsetName (FunctionDefinition_t *fd)
{
  fd->unsetName();
}


/**
 * Get the nth argument (bound variable) to the given FunctionDefinition_t
 * structure.
 * 
 * @param fd the FunctionDefinition_t structure
 *
 * @param n the index of the argument to return
 *
 * @return an ASTNode_t tree structure for the argument.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgument (const FunctionDefinition_t *fd, unsigned int n)
{
  return fd->getArgument(n);
}


/**
 * Get the argument named @p name to the given FunctionDefinition_t structure.
 *
 * @param fd the FunctionDefinition_t structure
 *
 * @param name the exact name (case-sensitive) of the sought-after argument
 *
 * @return an ASTNode_t structure representing the argument (bound
 * variable)
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgumentByName ( FunctionDefinition_t *fd,
                                       const char *name )
{
  return fd->getArgument(name ? name : "");
}


/**
 * Get the mathematical expression that constitutes the body of the given
 * function definition.
 *
 * @param fd the FunctionDefinition_t structure
 * 
 * @return an ASTNode_t tree that is the "math" subelement" of this
 * FunctionDefinition_t structure
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getBody (const FunctionDefinition_t *fd)
{
  return fd->getBody();
}


/**
 * Get the number of arguments that the given function definition takes.
 *
 * @param fd the FunctionDefinition_t structure
 * 
 * @return the number of arguments (bound variables) that must be passed
 * to this FunctionDefinition_t structure.
 */
LIBSBML_EXTERN
unsigned int
FunctionDefinition_getNumArguments (const FunctionDefinition_t *fd)
{
  return fd->getNumArguments();
}



/** @endcond doxygen-c-only */
