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


#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include "SBML.h"
#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "EventAssignment.h"


using namespace std;


/**
 * Creates a new EventAssignment, optionally with its variable and math
 * (via infix formula string) attributes set.
 */
EventAssignment::EventAssignment (  const string& variable
                                  , const string& formula  ) :
   SBase   ( variable )
 , mSBOTerm( -1       )
 , mMath   ( SBML_parseFormula( formula.c_str() ) )
{
}


/**
 * Creates a new EventAssignment with its variable and math attributes
 * set.
 */
EventAssignment::EventAssignment (const string& variable, const ASTNode* math)
 :
   SBase   ( variable )
 , mSBOTerm( -1       )
 , mMath   ( 0        )
{
  if (math) mMath = math->deepCopy();
}


/**
 * Copies this EventAssignment.
 */
EventAssignment::EventAssignment (const EventAssignment& rhs) :
   SBase   ( rhs )
 , mSBOTerm( -1  )
 , mMath   ( 0   )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Destroys this EventAssignment.
 */
EventAssignment::~EventAssignment ()
{
  delete mMath;
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
 * @return the sboTerm of this EventAssignment as an integer.  If not set,
 * sboTerm will be -1.  Use SBML::sboTermToString() to convert the sboTerm
 * to a zero-padded, seven digit string.
 */
int
EventAssignment::getSBOTerm () const
{
  return mSBOTerm;
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
 * @return true if the sboTerm of this EventAssignment has been set,
 * false otherwise.
 */
bool
EventAssignment::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
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
 * Sets the sboTerm field of this EventAssignment to value.
 */
void
EventAssignment::setSBOTerm (int sboTerm)
{
  mSBOTerm = sboTerm;
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
 * Unsets the sboTerm of this EventAssignment.
 */
void
EventAssignment::unsetSBOTerm ()
{
  mSBOTerm = -1;
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
EventAssignment::getElementName () const
{
  static const string name = "eventAssignment";
  return name;
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
    delete mMath;
    mMath = readMathML(stream);
    read  = true;
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

  //
  // variable: SId  { use="required" }  (L2v1, L2v2)
  //
  attributes.readInto("variable", mId);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (getVersion() == 2) mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
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

  //
  // variable: SId  { use="required" }  (L2v1, L2v2)
  //
  stream.writeAttribute("variable", mId);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (getVersion() == 2) SBML::writeSBOTerm(stream, mSBOTerm);
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
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
 * @return the sboTerm of this EventAssignment as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the sboTerm
 * to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
EventAssignment_getSBOTerm (const EventAssignment_t *ea)
{
  return ea->getSBOTerm();
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
 * @return true (non-zero) if the sboTerm of this EventAssignment has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetSBOTerm (const EventAssignment_t *ea)
{
  return static_cast<int>( ea->isSetSBOTerm() );
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
 * Sets the sboTerm field of this EventAssignment to value.
 */
LIBSBML_EXTERN
void
EventAssignment_setSBOTerm (EventAssignment_t *ea, int sboTerm)
{
  ea->setSBOTerm(sboTerm);
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


/**
 * Unsets the sboTerm of this EventAssignment.
 */
LIBSBML_EXTERN
void
EventAssignment_unsetSBOTerm (EventAssignment_t *ea)
{
  ea->unsetSBOTerm();
}
