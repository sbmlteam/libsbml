/**
 * \file    Constraint.cpp
 * \brief   SBML Constraint
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
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


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBML.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Constraint.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new Constraint
 * set.
 */
Constraint::Constraint () :
   SBase   ( -1 )
 , mMath   (  0 )
 , mMessage(  0 )
{
}


/**
 * Copy constructor - copies this Constraint.
 */
Constraint::Constraint (const Constraint& rhs) :
   SBase   ( rhs )
 , mMath   ( 0   )
 , mMessage( 0   )
{
  if (rhs.mMath)    mMath    = rhs.mMath->deepCopy();
  if (rhs.mMessage) mMessage = new XMLNode(*rhs.mMessage);
}


/**
 * Destroys this Constraint.
 */
Constraint::~Constraint ()
{
}


/**
 * Assignment operator
 */
Constraint& Constraint::operator=(const Constraint& rhs)
{
  this->SBase::operator =(rhs);

  if (rhs.mMath)    mMath    = rhs.mMath->deepCopy();
  if (rhs.mMessage) mMessage = new XMLNode(*rhs.mMessage);

  return *this;
}


/**
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


/**
 * @return a (deep) copy of this Constraint.
 */
SBase*
Constraint::clone () const
{
  return new Constraint(*this);
}


/**
 * @return the message for this Constraint.
 */
const XMLNode*
Constraint::getMessage () const
{
  return mMessage;
}


/**
 * @return the math for this Constraint.
 */
const ASTNode*
Constraint::getMath () const
{
  return mMath;
}


/**
 * @return true if the message of this Constraint has been set,
 * false otherwise.
 */
bool
Constraint::isSetMessage () const
{
  return isSetId();
}


/**
 * @return true if the math for this Constraint has been set,
 * false otherwise.
 */
bool
Constraint::isSetMath () const
{
  return (mMath != 0);
}


/**
 * Sets the message of this Constraint to a copy of xhtml.
 */
void
Constraint::setMessage (const XMLNode* xhtml)
{
  if (mMessage == xhtml) return;


  delete mMessage;
  mMessage = (xhtml != 0) ? new XMLNode(*xhtml) : 0;

}


/**
 * Sets the math of this Constraint to a copy of the given
 * ASTNode.
 */
void
Constraint::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;
}


/**
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


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Constraint::getElementName () const
{
  static const string name = "constraint";
  return name;
}


/**
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
    if (mMessage) mSBML->getErrorLog()->logError(21002);

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
  else if (name == "message")
  {
    delete mMessage;

    mMessage = new XMLNode(stream);
    checkXHTML(mMessage);
    read     = true;
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
    parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = deleteRDFAnnotation(mAnnotation);
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
Constraint::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
}


/**
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

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBML::writeSBOTerm(stream, mSBOTerm);
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Constraint::writeElements (XMLOutputStream& stream) const
{
  if (mMath) writeMathML(mMath, stream);
}




/**
 * @return a (deep) copy of this ListOfConstraints.
 */
SBase*
ListOfConstraints::clone () const
{
  return new ListOfConstraints(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfConstraints::getItemTypeCode () const
{
  return SBML_CONSTRAINT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfConstraints::getElementName () const
{
  static const string name = "listOfConstraints";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfConstraints::getElementPosition () const
{
  return 10;
}


/**
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
    object = new Constraint();
    mItems.push_back(object);
  }

  return object;
}




/**
 * Creates a new Constraint and returns a pointer to it.
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_create ()
{
  return new(nothrow) Constraint;
}


/**
 * Frees the given Constraint.
 */
LIBSBML_EXTERN
void
Constraint_free (Constraint_t *c)
{
  delete c;
}


/**
 * @return a (deep) copy of the given Constraint.
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_clone (const Constraint_t *c)
{
  return static_cast<Constraint*>( c->clone() );
}


/**
 * @return the symbol for this Constraint
 */
LIBSBML_EXTERN
const XMLNode_t *
Constraint_getMessage (const Constraint_t *c)
{
  return c->getMessage();
}


/**
 * @return the math for this Constraint.
 */
LIBSBML_EXTERN
const ASTNode_t *
Constraint_getMath (const Constraint_t *c)
{
  return c->getMath();
}


/**
 * @return true (non-zero) if the symbol of this Constraint has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Constraint_isSetMessage (const Constraint_t *c)
{
  return static_cast<int>( c->isSetMessage() );
}


/**
 * @return true (non-zero) if the math of this Constraint has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Constraint_isSetMath (const Constraint_t *c)
{
  return static_cast<int>( c->isSetMath() );
}


/**
 * Sets the symbol of this Constraint to a copy of xhtml.
 */
LIBSBML_EXTERN
void
Constraint_setMessage (Constraint_t *c, const XMLNode_t *xhtml)
{
  c->setMessage(xhtml);
}


/**
 * Sets the math of this Constraint to a copy of the given
 * ASTNode.
 */
LIBSBML_EXTERN
void
Constraint_setMath (Constraint_t *c, const ASTNode_t *math)
{
  c->setMath(math);
}
