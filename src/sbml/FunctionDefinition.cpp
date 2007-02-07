/**
 * \file    FunctionDefinition.cpp
 * \brief   SBML FunctionDefinition
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <cstring>

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include "SBMLVisitor.h"
#include "FunctionDefinition.h"
#include "SBMLDocument.h"
#include "Model.h"


using namespace std;


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
 * Creates a new FunctionDefinition, optionally with its id and math (via
 * an infix formula string) attributes set.
 */
FunctionDefinition::FunctionDefinition (  const string& id
                                        , const string& formula ) :
    SBase( id )
  , mMath( SBML_parseFormula( formula.c_str() ) )
 , mSBOTerm( -1 )
{
}


/**
 * Copies this FunctionDefinition.
 */
FunctionDefinition::FunctionDefinition (const FunctionDefinition& rhs) :
   SBase( rhs )
 , mMath( 0   )
 , mSBOTerm( rhs.mSBOTerm )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Destroys this FunctionDefinition.
 */
FunctionDefinition::~FunctionDefinition ()
{
  delete mMath;
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
 * @return the sboTerm of this FunctionDefinition as an integer.  If not
 * set, sboTerm will be -1.  Use SBML::sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
int
FunctionDefinition::getSBOTerm () const
{
  return mSBOTerm;
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
 * @return true if the sboTerm of this FunctionDefinition has been set,
 * false otherwise.
 */
bool
FunctionDefinition::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
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
 * Sets the sboTerm field of this FunctionDefinition to value.
 */
void
FunctionDefinition::setSBOTerm (int sboTerm)
{
  mSBOTerm = sboTerm;
}


/**
 * Unsets the sboTerm of this FunctionDefinition.
 */
void
FunctionDefinition::unsetSBOTerm ()
{
  mSBOTerm = -1;
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
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
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    checkAnnotation();
    read = true;
  }
  else if (name == "notes")
  {
    delete mNotes;
    mNotes = new XMLNode(stream);
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

  //
  // id: SId  { use="required" }  (L2v1, L2v2)
  //
  attributes.readInto("id", mId);
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  attributes.readInto("name", mName);

  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());

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

  //
  // id: SId  { use="required" }  (L2v1, L2v2)
  //
  stream.writeAttribute("id", mId);

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  stream.writeAttribute("name", mName);

  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  SBML::writeSBOTerm(stream, mSBOTerm);

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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
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




/**
 * Creates a new FunctionDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_create (void)
{
  return new(nothrow) FunctionDefinition("", "");
}


/**
 * Creates a new FunctionDefinition with the given id and math and returns
 * a pointer to it.  This convenience function is functionally equivalent
 * to:
 *
 *   fd = FunctionDefinition_create();
 *   FunctionDefinition_setId(fd, id); FunctionDefinition_setMath(fd, math);
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_createWith (const char *sid, ASTNode_t *math)
{
  return new(nothrow) FunctionDefinition(sid ? sid : "", math);
}


/**
 * Frees the given FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition_free (FunctionDefinition_t *fd)
{
  delete fd;
}


/**
 * @return the id of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getId (const FunctionDefinition_t *fd)
{
  return fd->isSetId() ? fd->getId().c_str() : NULL;
}


/**
 * @return the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getName (const FunctionDefinition_t *fd)
{
  return fd->isSetName() ? fd->getName().c_str() : NULL;
}


/**
 * @return the math of this FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getMath (const FunctionDefinition_t *fd)
{
  return fd->getMath();
}


/**
 * @return 1 if the id of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetId (const FunctionDefinition_t *fd)
{
  return static_cast<int>( fd->isSetId() );
}


/**
 * @return 1 if the name of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetName (const FunctionDefinition_t *fd)
{
  return static_cast<int>( fd->isSetName() );
}


/**
 * @return 1 if the math of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetMath (const FunctionDefinition_t *fd)
{
  return static_cast<int>( fd->isSetMath() );
}


/**
 * Sets the id of this FunctionDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setId (FunctionDefinition_t *fd, const char *sid)
{
  fd->setId(sid ? sid : "");
}


/**
 * Sets the name of this FunctionDefinition to a copy of name.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setName (FunctionDefinition_t *fd, const char *name)
{
  (name == NULL) ? fd->unsetName() : fd->setName(name);
}


/**
 * Sets the math of this FunctionDefinition to the given ASTNode.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setMath (FunctionDefinition_t *fd, const ASTNode_t *math)
{
  fd->setMath(math);
}


/**
 * Unsets the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition_unsetName (FunctionDefinition_t *fd)
{
  fd->unsetName();
}


/**
 * @return the nth argument (bound variable) passed to this
 * FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgument (const FunctionDefinition_t *fd, unsigned int n)
{
  return fd->getArgument(n);
}


/**
 * @return the argument (bound variable) in this FunctionDefinition with
 * the given name or NULL if no such argument exists.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgumentByName ( FunctionDefinition_t *fd,
                                       const char *name )
{
  return fd->getArgument(name ? name : "");
}


/**
 * @return the body of this FunctionDefinition, or NULL if no body is
 * defined.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getBody (const FunctionDefinition_t *fd)
{
  return fd->getBody();
}


/**
 * @return the number of arguments (bound variables) that must be passed
 * to this FunctionDefinition.
 */
LIBSBML_EXTERN
unsigned int
FunctionDefinition_getNumArguments (const FunctionDefinition_t *fd)
{
  return fd->getNumArguments();
}
