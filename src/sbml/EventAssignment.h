/**
 * \file    EventAssignment.h
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


#ifndef EventAssignment_h
#define EventAssignment_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN EventAssignment : public SBase
{
public:

  /**
   * Creates a new EventAssignment, optionally with its variable and math
   * (via infix formula string) attributes set.
   */
  EventAssignment (  const std::string& variable = ""
                   , const std::string& formula  = "" );
  /**
   * Creates a new EventAssignment with its variable and math attributes
   * set.
   */
  EventAssignment (const std::string& variable, const ASTNode* math);

  /**
   * Destroys this EventAssignment.
   */
  virtual ~EventAssignment ();

  /**
   * Copy constructor. Creates a copy of this EventAssignment.
   */
  EventAssignment (const EventAssignment& rhs);

  /**
   * Assignment operator
   */
  EventAssignment& operator=(const EventAssignment& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Event's next
   * EventAssignment (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this EventAssignment.
   */
  virtual SBase* clone () const;


  /**
   * @return the variable of this EventAssignment.
   */
  const std::string& getVariable () const;

  /**
   * @return the math of this EventAssignment.
   */
  const ASTNode* getMath () const;


  /**
   * @return true if the variable of this EventAssignment has been set, false
   * otherwise.
   */
  bool isSetVariable () const;

  /**
   * @return true if the math of this EventAssignment has been set, false
   * otherwise.
   */
  bool isSetMath () const;


  /**
   * Sets the variable of this EventAssignment to a copy of sid.
   */
  void setVariable (const std::string& sid);

  /**
   * Sets the math of this EventAssignment to a copy of the given ASTNode.
   */
  void setMath (const ASTNode* math);


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * @return the name of this element ie "eventAssignment".
   */
  virtual const std::string& getElementName () const;

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


protected:

  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  ASTNode*  mMath;
};



class LIBSBML_EXTERN ListOfEventAssignments : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfEventAssignments.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
 * @return the name of this element ie "listOfEventAssignments".
   */
  virtual const std::string& getElementName () const;

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new EventAssignment and returns a pointer to it.
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_create (void);

/**
 * Creates a new EventAssignment with the given variable and math and
 * returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ea = EventAssignment_create();\n
 *   EventAssignment_setVariable(ea, variable);\n
 *   EventAssignment_setMath(ea, math);\n
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_createWith (const char *variable, ASTNode_t *math);

/**
 * Creates a new EventAssignment with the given variable and formula and
 * returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ea = EventAssignment_create();\n
 *   EventAssignment_setVariable(ea, variable);\n
 *   EventAssignment_setFormula(ea, formula);\n
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_createWithFormula (const char *variable, const char *formula);

/**
 * Frees the given EventAssignment.
 */
LIBSBML_EXTERN
void
EventAssignment_free (EventAssignment_t *ea);

/**
 * @return a (deep) copy of this EventAssignment.
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_clone (const EventAssignment_t *ea);


/**
 * @return the variable of this EventAssignment.
 */
LIBSBML_EXTERN
const char *
EventAssignment_getVariable (const EventAssignment_t *ea);

/**
 * @return the math of this EventAssignment.
 */
LIBSBML_EXTERN
const ASTNode_t *
EventAssignment_getMath (const EventAssignment_t *ea);


/**
 * @return 1 if the variable of this EventAssignment has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetVariable (const EventAssignment_t *ea);

/**
 * @return 1 if the math of this EventAssignment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetMath (const EventAssignment_t *ea);


/**
 * Sets the variable of this EventAssignment to a copy of sid.
 */
LIBSBML_EXTERN
void
EventAssignment_setVariable (EventAssignment_t *ea, const char *sid);

/**
 * Sets the math of this EventAssignment to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
EventAssignment_setMath (EventAssignment_t *ea, const ASTNode_t *math);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* EventAssignment_h */
