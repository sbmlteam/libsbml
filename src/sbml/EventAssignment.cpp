/**
 * \file    EventAssignment.cpp
 * \brief   SBML EventAssignment
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


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBML.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/EventAssignment.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new EventAssignment, optionally with its variable and math
 * (via infix formula string) attributes set.
 */
EventAssignment::EventAssignment (  const string& variable
                                  , const string& formula  ) :
   SBase   ( variable, "", -1 )
 , mMath   ( SBML_parseFormula( formula.c_str() ) )
{
}


/**
 * Creates a new EventAssignment with its variable and math attributes
 * set.
 */
EventAssignment::EventAssignment (const string& variable, const ASTNode* math)
 :
   SBase   ( variable, "", -1 )
 , mMath   ( 0        )
{
  if (math) mMath = math->deepCopy();
}


/**
 * Destroys this EventAssignment.
 */
EventAssignment::~EventAssignment ()
{
  delete mMath;
}


/**
 * Copy constructor. Creates a copy of this EventAssignment.
 */
EventAssignment::EventAssignment (const EventAssignment& rhs) :
   SBase   ( rhs )
 , mMath   ( 0   )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Assignment operator
 */
EventAssignment& EventAssignment::operator=(const EventAssignment& rhs)
{
  this->SBase::operator =(rhs);
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Event's next
 * EventAssignment (if available).
 */
bool
EventAssignment::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this EventAssignment.
 */
SBase*
EventAssignment::clone () const
{
  return new EventAssignment(*this);
}


/**
 * @return the variable of this EventAssignment.
 */
const string&
EventAssignment::getVariable () const
{
  return getId();
}


/**
 * @return the math of this EventAssignment.
 */
const ASTNode*
EventAssignment::getMath () const
{
  return mMath;
}


/**
 * @return true if the variable of this EventAssignment has been set, false
 * otherwise.
 */
bool
EventAssignment::isSetVariable () const
{
  return isSetId();
}


/**
 * @return true if the math of this EventAssignment has been set, false
 * otherwise.
 */
bool
EventAssignment::isSetMath () const
{
  return (mMath != 0);
}


/**
 * Sets the variable of this EventAssignment to a copy of sid.
 */
void
EventAssignment::setVariable (const string& sid)
{
  setId(sid);
}


/**
 * Sets the math of this EventAssignment to a copy of the given ASTNode.
 */
void
EventAssignment::setMath (const ASTNode* math)
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
EventAssignment::getTypeCode () const
{
  return SBML_EVENT_ASSIGNMENT;
}


/**
 * @return the name of this element ie "eventAssignment".
 */
const string&
EventAssignment::getElementName () const
{
  static const string name = "eventAssignment";
  return name;
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
EventAssignment::writeElements (XMLOutputStream& stream) const
{
  if (mMath) writeMathML(mMath, stream);
}


/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
EventAssignment::readOtherXML (XMLInputStream& stream)
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
EventAssignment::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // variable: SId  { use="required" }  (L2v1, L2v2)
  //
  attributes.readInto("variable", mId);
  SBase::checkIdSyntax();


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
EventAssignment::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // variable: SId  { use="required" }  (L2v1, L2v2)
  //
  stream.writeAttribute("variable", mId);


  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBML::writeSBOTerm(stream, mSBOTerm);
}


/**
 * @return a (deep) copy of this ListOfEventAssignments.
 */
SBase*
ListOfEventAssignments::clone () const
{
  return new ListOfEventAssignments(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfEventAssignments::getItemTypeCode () const
{
  return SBML_EVENT_ASSIGNMENT;
}


/**
 * @return the name of this element ie "listOfEventAssignments".
 */
const string&
ListOfEventAssignments::getElementName () const
{
  static const string name = "listOfEventAssignments";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its
 * siblings or -1 (default) to indicate the position is not significant.
 */
int
ListOfEventAssignments::getElementPosition () const
{
  return 3;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfEventAssignments::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "eventAssignment")
  {
    object = new EventAssignment();
    mItems.push_back(object);
  }

  return object;
}




/**
 * Creates a new EventAssignment and returns a pointer to it.
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_create (void)
{
  return new(nothrow) EventAssignment;
}


/**
 * Creates a new EventAssignment with the given variable and math and
 * returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ea = EventAssignment_create();
 *   EventAssignment_setId(ea, variable);
 *   EventAssignment_setMath(ea, math);
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_createWith (const char *variable, ASTNode_t* math)
{
  return new(nothrow) EventAssignment(variable ? variable : "", math);
}


/**
 * Frees the given EventAssignment.
 */
LIBSBML_EXTERN
void
EventAssignment_free (EventAssignment_t *ea)
{
  delete ea;
}


/**
 * @return a (deep) copy of this EventAssignment.
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_clone (const EventAssignment_t *ea)
{
  return static_cast<EventAssignment*>( ea->clone() );
}


/**
 * @return the variable of this EventAssignment.
 */
LIBSBML_EXTERN
const char *
EventAssignment_getVariable (const EventAssignment_t *ea)
{
  return ea->isSetVariable() ? ea->getVariable().c_str() : NULL;
}


/**
 * @return the math of this EventAssignment.
 */
LIBSBML_EXTERN
const ASTNode_t *
EventAssignment_getMath (const EventAssignment_t *ea)
{
  return ea->getMath();
}


/**
 * @return 1 if the variable of this EventAssignment has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetVariable (const EventAssignment_t *ea)
{
  return static_cast<int>( ea->isSetVariable() );
}


/**
 * @return 1 if the math of this EventAssignment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetMath (const EventAssignment_t *ea)
{
  return static_cast<int>( ea->isSetMath() );
}


/**
 * Sets the id of this EventAssignment to a copy of sid.
 */
LIBSBML_EXTERN
void
EventAssignment_setVariable (EventAssignment_t *ea, const char *sid)
{
  ea->setVariable(sid ? sid : "");
}


/**
 * Sets the math of this EventAssignment to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
EventAssignment_setMath (EventAssignment_t *ea, const ASTNode_t *math)
{
  ea->setMath(math);
}
