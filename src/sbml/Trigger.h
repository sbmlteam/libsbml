/**
 * @file    Trigger.h
 * @brief   Definition of Trigger
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
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
 * @brief LibSBML implementation of %SBML's %Trigger construct for %Event.
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
 * @see Event
 * @see Delay
 * @see EventAssignment
 */


#ifndef Trigger_h
#define Trigger_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN Trigger : public SBase
{
public:

  /**
   * Creates a new Trigger using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this Trigger
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Trigger
   * 
   * @note Once a Trigger has been added to an SBMLDocument, the @p level,
   * @p version for the document @em override those used
   * to create the Trigger.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  Trigger (unsigned int level, unsigned int version);


  /**
   * Creates a new Trigger using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp; Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Once a Trigger has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the Trigger.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  Trigger (SBMLNamespaces* sbmlns);


  /**
   * Creates a new Trigger using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp; Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Once a Trigger has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the Trigger.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
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
  Trigger (SBMLNamespaces* sbmlns);


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
  virtual Trigger* clone () const;


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
   * @return @c true if the formula (meaning the "math" subelement) of
   * this Trigger has been set, @c false otherwise.
   */
  bool isSetMath () const;


  /**
   * Sets the trigger expression of this Trigger instance to a copy of the given
   * ASTNode.
   *
   * @param math an ASTNode representing a formula tree.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_OBJECT!
   */
  int setMath (const ASTNode* math);

  /** @cond doxygen-libsbml-internal */

  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument to use.
   */
  virtual void setSBMLDocument (SBMLDocument* d);

  /**
   * Sets the parent SBML object of this SBML object.
   *
   * @param sb the SBML object to use
   */
  virtual void setParentSBMLObject (SBase* sb);

  /** @endcond doxygen-libsbml-internal */


  /**
   * Returns the libSBML type code of this object instance.
   *
   * @if clike LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.
   * The set of possible type codes is defined in the enumeration
   * #SBMLTypeCode_t.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if java LibSBML attaches an
   * identifying code to every kind of SBML object.  These are known as
   * <em>SBML type codes</em>.  In other languages, the set of type codes
   * is stored in an enumeration; in the Java language interface for
   * libSBML, the type codes are defined as static integer constants in
   * interface class {@link libsbmlConstants}.  The names of the type codes
   * all begin with the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or @c SBML_UNKNOWN (default).
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
  /** @endcond doxygen-libsbml-internal */


  /** @cond doxygen-libsbml-internal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond doxygen-libsbml-internal */


  /**
   * Predicate returning @c true or @c false depending on whether
   * all the required elements for this Trigger object
   * have been set.
   *
   * @note The required elements for a Trigger object are:
   * math
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const ;


protected:
  /** @cond doxygen-libsbml-internal */

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  Trigger ();


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

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;


  /** @endcond doxygen-libsbml-internal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
Trigger_t *
Trigger_create (unsigned int level, unsigned int version);


LIBSBML_EXTERN
Trigger_t *
Trigger_createWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
void
Trigger_free (Trigger_t *t);


LIBSBML_EXTERN
Trigger_t *
Trigger_clone (const Trigger_t *t);


LIBSBML_EXTERN
const XMLNamespaces_t *
Trigger_getNamespaces(Trigger_t *c);


LIBSBML_EXTERN
const ASTNode_t *
Trigger_getMath (const Trigger_t *t);


LIBSBML_EXTERN
int
Trigger_isSetMath (const Trigger_t *t);


LIBSBML_EXTERN
int
Trigger_setMath (Trigger_t *t, const ASTNode_t *math);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* Trigger_h */
