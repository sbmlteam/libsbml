/**
 * @file    Trigger.h
 * @brief   Definition of Trigger
 * @author  Sarah Keating
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
 *------------------------------------------------------------------------- -->
 *
 * @class Trigger
 * @brief LibSBML implementation of %SBML's Trigger construct for Event.
 *
 * An Event object defines when the event can occur, the variables that
 * are affected by the event, and how the variables are affected.  The
 * Trigger construct in SBML is used to define a mathematical expression
 * that determines when an Event @em fires.
 *
 * A Trigger contains one subelement named "math" containing a MathML
 * expression.  The expression must evaluate to a value of type @c boolean.
 * The exact moment at which the expression evaluates to @c true is the
 * time point when the Event is @em fired.
 * 
 * An event only fires when its Trigger expression makes the transition in
 * value from @c false to @c true.  The event will also fire at any future
 * time points when the trigger expression makes this transition; in other
 * words, an event can fire multiple times during a simulation if its
 * trigger condition makes the transition from @c false to @c true more
 * than once.
 * 
 * An important question is whether an event can fire prior to, or at,
 * initial simulation time, that is <em>t &lt; 0</em>.  The answer is no:
 * an event can only be triggered immediately after initial simulation time
 * i.e., <em>t &gt; 0</em>.
 *
 * @see Event, Delay, EventAssignment.
 */


#ifndef Trigger_h
#define Trigger_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>


class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN Trigger : public SBase
{
public:

  /**
   * Creates a new Trigger, optionally with the given math. 
   *
   * @param math an ASTNode representing the mathematical formula for
   * the trigger expression.
   *
   * @docnote The native C++ implementation of this method defines a
   * default argument value.  In the documentation generated for different
   * libSBML language bindings, you may or may not see corresponding
   * arguments in the method declarations.  For example, in Java, a default
   * argument is handled by declaring two separate methods, with one of
   * them having the argument and the other one lacking the argument.
   * However, the libSBML documentation will be @em identical for both
   * methods.  Consequently, if you are reading this and do not see an
   * argument even though one is described, please look for descriptions of
   * other variants of this method near where this one appears in the
   * documentation.
   */
  Trigger (const ASTNode* math = NULL);


  /**
   * Destroys this Trigger.
   */
  virtual ~Trigger ();


  /**
   * Copy constructor; creates a copy of this Trigger.
   */
  Trigger (const Trigger& orig);


  /**
   * Assignment operator
   */
  Trigger& operator=(const Trigger& rhs);


  /**
   * Accepts the given SBMLVisitor for this instance of Trigger.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this Trigger.
   *
   * @return a (deep) copy of this Trigger.
   */
  virtual SBase* clone () const;


  /**
   * Get the mathematical formula for the trigger and return it
   * as an AST.
   * 
   * @return the math of this Trigger.
   */
  const ASTNode* getMath () const;


  /**
   * Predicate to test whether the math for this trigger has been set.
   *
   * 
   * @return @c true if the formula (meaning the @c math subelement) of
   * this Trigger has been set, @c false otherwise.
   */
  bool isSetMath () const;


  /**
   * Sets the trigger expression of this Trigger instance to a copy of the given
   * ASTNode.
   *
   * @param math an ASTNode representing a formula tree.
   */
  void setMath (const ASTNode* math);


  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument to use.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /**
   * Returns the libSBML type code of this object instance.
   *
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN 
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Returns the XML element name of this object, which for Trigger, is
   * always @c "trigger".
   * 
   * @return the name of this element, i.e., @c "trigger". 
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */

  /**
   * Returns the position of this element.
   * 
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond doxygen-libsbml-internal */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
//  virtual SBase* createObject (XMLInputStream& stream);

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


  ASTNode*     mMath;

  /** @endcond doxygen-libsbml-internal */
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
Trigger_t *
Trigger_create (void);


LIBSBML_EXTERN
Trigger_t *
Trigger_createWithMath ( const ASTNode_t *math);


LIBSBML_EXTERN
void
Trigger_free (Trigger_t *t);


LIBSBML_EXTERN
SBase_t *
Trigger_clone (const Trigger_t *t);


LIBSBML_EXTERN
const ASTNode_t *
Trigger_getMath (const Trigger_t *t);


LIBSBML_EXTERN
int
Trigger_isSetMath (const Trigger_t *t);


LIBSBML_EXTERN
void
Trigger_setMath (Trigger_t *t, const ASTNode_t *math);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Trigger_h */
