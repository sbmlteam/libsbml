/**
 * @file    LocalParameter.h
 * @brief   Definitions of LocalParameter and ListOfLocalParameters.
 * @author  Sarah Keating
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 * 
 * @class LocalParameter.
 * @brief LibSBML implementation of %SBML's %LocalParameter construct.
 *
 * A LocalParameter is used in %SBML to define a symbol associated with a value;
 * this symbol can then be used in mathematical formulas in a model.  By
 * default, parameters have constant value for the duration of a
 * simulation, and for this reason are called @em parameters instead of @em
 * variables in %SBML, although it is crucial to understand that <em>%SBML
 * parameters represent both concepts</em>.  Whether a given %SBML
 * parameter is intended to be constant or variable is indicated by the
 * value of its "constant" attribute.
 * 
 * %SBML's Parameter has one required attribute, "id", to give the
 * parameter a unique identifier by which other parts of an %SBML model
 * definition can refer to it.  A parameter can also have an optional
 * "name" attribute of type @c string.  Identifiers and names must be used
 * according to the guidelines described in the %SBML specification (e.g.,
 * Section 3.3 in the Level&nbsp;2 Version&nbsp;4 specification).
 * 
 * The optional attribute "value" determines the value (of type @c double)
 * assigned to the identifier.  A missing value for "value" implies that
 * the value either is unknown, or to be obtained from an external source,
 * or determined by an initial assignment.  The units associated with the
 * value of the parameter are specified by the attribute named "units".
 * The value assigned to the parameter's "units" attribute must be chosen
 * from one of the following possibilities: one of the base unit
 * identifiers defined in %SBML; one of the built-in unit identifiers @c
 * "substance", @c "time", @c "volume", @c "area" or @c "length"; or the
 * identifier of a new unit defined in the list of unit definitions in the
 * enclosing Model structure.  There are no constraints on the units that
 * can be chosen from these sets.  There are no default units for
 * parameters.  Please consult the %SBML specification documents for more
 * details about the meanings and implications of the various unit choices.
 * 
 * The Parameter structure has an optional boolean attribute named
 * "constant" that indicates whether the parameter's value can vary during
 * a simulation.  The attribute's default value is @c true.  A value of @c
 * false indicates the parameter's value can be changed by Rule constructs
 * and that the "value" attribute is actually intended to be the initial
 * value of the parameter. Parameters local to a reaction (that is, those
 * defined within the KineticLaw structure of a Reaction) cannot be changed
 * by rules and therefore are implicitly always constant; thus, parameter
 * definitions within Reaction structures should @em not have their
 * "constant" attribute set to @c false.
 * 
 * What if a global parameter has its "constant" attribute set to @c false,
 * but the model does not contain any rules, events or other constructs
 * that ever change its value over time?  Although the model may be
 * suspect, this situation is not strictly an error.  A value of @c false
 * for "constant" only indicates that a parameter @em can change value, not
 * that it @em must.
 *
 * As with all other major %SBML components, Parameter is derived from
 * SBase, and the methods defined on SBase are available on Parameter.
 *
 * @see ListOfParameters
 * @see KineticLaw
 * 
 * @note The use of the term @em parameter in %SBML sometimes leads to
 * confusion among readers who have a particular notion of what something
 * called "parameter" should be.  It has been the source of heated debate,
 * but despite this, no one has yet found an adequate replacement term that
 * does not have different connotations to different people and hence leads
 * to confusion among @em some subset of users.  Perhaps it would have been
 * better to have two constructs, one called @em constants and the other
 * called @em variables.  The current approach in %SBML is simply more
 * parsimonious, using a single Parameter construct with the boolean flag
 * "constant" indicating which flavor it is.  In any case, readers are
 * implored to look past their particular definition of a @em parameter and
 * simply view %SBML's Parameter as a single mechanism for defining both
 * constants and (additional) variables in a model.  (We write @em
 * additional because the species in a model are usually considered to be
 * the central variables.)  After all, software tools are not required to
 * expose to users the actual names of particular %SBML constructs, and
 * thus tools can present to their users whatever terms their designers
 * feel best matches their target audience.
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class ListOfParameters.
 * @brief LibSBML implementation of SBML's %ListOfParameters construct.
 * 
 * The various ListOf___ classes in %SBML are merely containers used for
 * organizing the main components of an %SBML model.  All are derived from
 * the abstract class SBase, and inherit the various attributes and
 * subelements of SBase, such as "metaid" as and "annotation".  The
 * ListOf___ classes do not add any attributes of their own.
 *
 * The relationship between the lists and the rest of an %SBML model is
 * illustrated by the following (for %SBML Level&nbsp;2 Version&nbsp;4):
 *
 * @image html listof-illustration.jpg "ListOf___ elements in an SBML Model"
 * @image latex listof-illustration.jpg "ListOf___ elements in an SBML Model"
 *
 * Readers may wonder about the motivations for using the ListOf___
 * containers.  A simpler approach in XML might be to place the components
 * all directly at the top level of the model definition.  We chose instead
 * to group them within XML elements named after ListOf<em>Classname</em>,
 * in part because we believe this helps organize the components and makes
 * visual reading of models in XML easier.  More importantly, the fact that
 * the container classes are derived from SBase means that software tools
 * can add information about the lists themselves into each list
 * container's "annotation".
 *
 * @see ListOfFunctionDefinitions
 * @see ListOfUnitDefinitions
 * @see ListOfCompartmentTypes
 * @see ListOfSpeciesTypes
 * @see ListOfCompartments
 * @see ListOfSpecies
 * @see ListOfParameters
 * @see ListOfInitialAssignments
 * @see ListOfRules
 * @see ListOfConstraints
 * @see ListOfReactions
 * @see ListOfEvents
 */

#ifndef LocalParameter_h
#define LocalParameter_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/Parameter.h>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


#ifdef __cplusplus


#include <string>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLVisitor;


class LIBSBML_EXTERN LocalParameter : public Parameter
{
public:

  /**
   * Creates a new LocalParameter using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this LocalParameter
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * LocalParameter
   * 
   * @note Once a LocalParameter has been added to an SBMLDocument, the @p level,
   * @p version for the document @em override those used
   * to create the LocalParameter.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  LocalParameter (unsigned int level, unsigned int version);


  /**
   * Creates a new LocalParameter using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp; Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * It is worth emphasizing that although this constructor does not take
   * an identifier argument, in SBML Level&nbsp;2 and beyond, the "id"
   * (identifier) attribute of a LocalParameter is required to have a value.
   * Thus, callers are cautioned to assign a value after calling this
   * constructor if no identifier is provided as an argument.  Setting the
   * identifier can be accomplished using the method @if clike
   * SBase::setId(). @endif@if java SBase::setId(String id).
   * @endif
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Once a LocalParameter has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the LocalParameter.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  LocalParameter (SBMLNamespaces* sbmlns);


  /**
   * Destroys this LocalParameter.
   */
  virtual ~LocalParameter ();


  /**
   * Copy constructor; creates a copy of a LocalParameter.
   * 
   * @param orig the LocalParameter instance to copy.
   */
  LocalParameter(const LocalParameter& orig);


  /**
   * Copy constructor; creates a LocalParameter that copies
   * attributes from the Parameter.
   * 
   * @param orig the Parameter instance to copy.
   */
  LocalParameter(const Parameter& orig);


  /**
   * Assignment operator for LocalParameter.
   */
  LocalParameter& operator=(const LocalParameter& orig);


  /**
   * Accepts the given SBMLVisitor for this instance of LocalParameter.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next LocalParameter in the list
   * of parameters within which this LocalParameter is embedded (i.e., either
   * the list of parameters in the parent Model or the list of parameters
   * in the enclosing KineticLaw).
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this LocalParameter.
   * 
   * @return a (deep) copy of this LocalParameter.
   */
  virtual LocalParameter* clone () const;


  /**
   * Constructs and returns a UnitDefinition that corresponds to the units
   * of this LocalParameter's value.
   *
   * LocalParameters in SBML have an attribute ("units") for declaring the units
   * of measurement intended for the parameter's value.  <b>No defaults are
   * defined</b> by SBML in the absence of a definition for "units".  The
   * LocalParameter::getDerivedUnitDefinition() method returns a UnitDefinition
   * object based on the units declared for this LocalParameter using its
   * "units" attribute, or it returns NULL if no units have been declared.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return NULL.
   *
   * Note that unit declarations for LocalParameter are in terms of the @em
   * identifier of a unit, but this method returns a UnitDefinition object,
   * not a unit identifier.  It does this by constructing an appropriate
   * UnitDefinition even when the value of the "units" attribute is one of
   * the predefined SBML units @c "substance", @c "volume", @c "area", @c
   * "length" or @c "time".  Callers may find this particularly useful
   * when used in conjunction with the helper methods on UnitDefinition
   * for comparing different UnitDefinition objects.
   *
   * @return a UnitDefinition that expresses the units of this 
   * LocalParameter.
   */
  UnitDefinition * getDerivedUnitDefinition();


  /**
   * Constructs and returns a UnitDefinition that corresponds to the units
   * of this LocalParameter's value.
   *
   * LocalParameters in SBML have an attribute ("units") for declaring the units
   * of measurement intended for the parameter's value.  <b>No defaults are
   * defined</b> by SBML in the absence of a definition for "units".  The
   * LocalParameter::getDerivedUnitDefinition() method returns a UnitDefinition
   * object based on the units declared for this LocalParameter using its
   * "units" attribute, or it returns NULL if no units have been declared.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return NULL.
   *
   * Note that unit declarations for LocalParameter are in terms of the @em
   * identifier of a unit, but this method returns a UnitDefinition object,
   * not a unit identifier.  It does this by constructing an appropriate
   * UnitDefinition even when the value of the "units" attribute is one of
   * the predefined SBML units @c "substance", @c "volume", @c "area", @c
   * "length" or @c "time".  Callers may find this particularly useful
   * when used in conjunction with the helper methods on UnitDefinition
   * for comparing different UnitDefinition objects.
   *
   * @return a UnitDefinition that expresses the units of this 
   * LocalParameter.
   */
  const UnitDefinition * getDerivedUnitDefinition() const;


  /**
   * Returns the libSBML type code for this %SBML object.
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
   * Returns the XML element name of this object, which for LocalParameter, is
   * always @c "localParameter".
   * 
   * @return the name of this element, i.e., @c "localParameter".
   */
  virtual const std::string& getElementName () const;


  /**
   * Predicate returning @c true or @c false depending on whether
   * all the required attributes for this LocalParameter object
   * have been set.
   *
   * @note The required attributes for a LocalParameter object are:
   * id (name in L1); value (L1V1 only)
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const ;


protected:
  /** @cond doxygen-libsbml-internal */

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  LocalParameter ();


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


class LIBSBML_EXTERN ListOfLocalParameters : public ListOfParameters
{
public:

  /**
   * Creates and returns a deep copy of this ListOfLocalParameters instance.
   *
   * @return a (deep) copy of this ListOfLocalParameters.
   */
  virtual ListOfLocalParameters* clone () const;


  /**
   * Returns the libSBML type code for this %SBML object.
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
  virtual SBMLTypeCode_t getTypeCode () const { return SBML_LIST_OF; };


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., LocalParameter objects, if the list is non-empty).
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
   * @return the SBML type code for the objects contained in this ListOf
   * instance, or @c SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;


  /**
   * Returns the XML element name of this object.
   *
   * For ListOfLocalParameters, the XML element name is @c "listOfLocalParameters".
   * 
   * @return the name of this element, i.e., @c "listOfLocalParameters".
   */
  virtual const std::string& getElementName () const;


  /**
   * Get a LocalParameter from the ListOfLocalParameters.
   *
   * @param n the index number of the LocalParameter to get.
   * 
   * @return the nth LocalParameter in this ListOfLocalParameters.
   *
   * @see size()
   */
  virtual LocalParameter * get(unsigned int n); 


  /**
   * Get a LocalParameter from the ListOfLocalParameters.
   *
   * @param n the index number of the LocalParameter to get.
   * 
   * @return the nth LocalParameter in this ListOfLocalParameters.
   *
   * @see size()
   */
  virtual const LocalParameter * get(unsigned int n) const; 


  /**
   * Get a LocalParameter from the ListOfLocalParameters
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the LocalParameter to get.
   * 
   * @return LocalParameter in this ListOfLocalParameters
   * with the given id or NULL if no such
   * LocalParameter exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual LocalParameter* get (const std::string& sid);


  /**
   * Get a LocalParameter from the ListOfLocalParameters
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the LocalParameter to get.
   * 
   * @return LocalParameter in this ListOfLocalParameters
   * with the given id or NULL if no such
   * LocalParameter exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const LocalParameter* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfLocalParameters items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   *
   * @see size()
   */
  virtual LocalParameter* remove (unsigned int n);


  /**
   * Removes item in this ListOfLocalParameters items with the given identifier.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then @c
   * NULL is returned.
   *
   * @param sid the identifier of the item to remove
   *
   * @return the item removed.  As mentioned above, the caller owns the
   * returned item.
   */
  virtual LocalParameter* remove (const std::string& sid);


  /** @cond doxygen-libsbml-internal */

  /**
   * Get the ordinal position of this element in the containing object
   * (which in this case is the Model object).
   *
   * The ordering of elements in the XML form of %SBML is generally fixed
   * for most components in %SBML.  So, for example, the ListOfLocalParameters
   * in a model is (in %SBML Level&nbsp;2 Version&nbsp;4) the seventh
   * ListOf___.  (However, it differs for different Levels and Versions of
   * SBML.)
   *
   * @return the ordinal position of the element with respect to its
   * siblings, or @c -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /** @endcond doxygen-libsbml-internal */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Create a ListOfLocalParameters object corresponding to the next token in
   * the XML input stream.
   * 
   * @return the %SBML object corresponding to next XMLToken in the
   * XMLInputStream, or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

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
LocalParameter_t *
LocalParameter_create (unsigned int level, unsigned int version);


LIBSBML_EXTERN
LocalParameter_t *
LocalParameter_createWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
void
LocalParameter_free (LocalParameter_t *p);


LIBSBML_EXTERN
LocalParameter_t *
LocalParameter_clone (const LocalParameter_t *p);


LIBSBML_EXTERN
void
LocalParameter_initDefaults (LocalParameter_t *p);


LIBSBML_EXTERN
const XMLNamespaces_t *
LocalParameter_getNamespaces(LocalParameter_t *c);


LIBSBML_EXTERN
const char *
LocalParameter_getId (const LocalParameter_t *p);


LIBSBML_EXTERN
const char *
LocalParameter_getName (const LocalParameter_t *p);


LIBSBML_EXTERN
double
LocalParameter_getValue (const LocalParameter_t *p);


LIBSBML_EXTERN
const char *
LocalParameter_getUnits (const LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_getConstant (const LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_isSetId (const LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_isSetName (const LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_isSetValue (const LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_isSetUnits (const LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_setId (LocalParameter_t *p, const char *sid);


LIBSBML_EXTERN
int
LocalParameter_setName (LocalParameter_t *p, const char *name);


LIBSBML_EXTERN
int
LocalParameter_setValue (LocalParameter_t *p, double value);


LIBSBML_EXTERN
int
LocalParameter_setUnits (LocalParameter_t *p, const char *units);


LIBSBML_EXTERN
int
LocalParameter_setConstant (LocalParameter_t *p, int value);


LIBSBML_EXTERN
int
LocalParameter_unsetName (LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_unsetValue (LocalParameter_t *p);


LIBSBML_EXTERN
int
LocalParameter_unsetUnits (LocalParameter_t *p);


LIBSBML_EXTERN
UnitDefinition_t * 
LocalParameter_getDerivedUnitDefinition(LocalParameter_t *p);


LIBSBML_EXTERN
LocalParameter_t *
ListOfLocalParameters_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
LocalParameter_t *
ListOfLocalParameters_removeById (ListOf_t *lo, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* LocalParameter_h */
