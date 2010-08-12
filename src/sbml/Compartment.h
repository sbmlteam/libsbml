/**
 * @file    Compartment.h
 * @brief   Definitions of Compartment and ListOfCompartments
 * @author  Ben Bornstein
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class Compartment
 * @brief  LibSBML implementation of SBML's %Compartment construct.
 *
 * A compartment in SBML represents a bounded space in which species are
 * located.  Compartments do not necessarily have to correspond to actual
 * structures inside or outside of a biological cell.
 * 
 * It is important to note that although compartments are optional in the
 * overall definition of Model, every species in an SBML model must be
 * located in a compartment.  This in turn means that if a model defines
 * any species, the model must also define at least one compartment.  The
 * reason is simply that species represent physical things, and therefore
 * must exist @em somewhere.  Compartments represent the @em somewhere.
 *
 * Compartment has one required attribute, "id", to give the compartment a
 * unique identifier by which other parts of an SBML model definition can
 * refer to it.  A compartment can also have an optional "name" attribute
 * of type @c string.  Identifiers and names must be used according to the
 * guidelines described in the SBML specifications.
 *
 * In SBML Level 2, each compartment in a model may optionally be
 * designated as belonging to a particular compartment type.  The optional
 * attribute "compartmentType" is used identify the compartment type
 * represented by the Compartment structure.  The "compartmentType"
 * attribute's value must be the identifier of a CompartmentType instance
 * defined in the model.  If the "compartmentType" attribute is not present
 * on a particular compartment definition, a unique virtual compartment
 * type is assumed for that compartment, and no other compartment can
 * belong to that compartment type.  The values of "compartmentType"
 * attributes on compartments have no effect on the numerical
 * interpretation of a model.  Simulators and other numerical analysis
 * software may ignore "compartmentType" attributes.  The "compartmentType"
 * attribute and the CompartmentType class of objects are not present
 * in SBML Level&nbsp;3 Core nor in SBML Level&nbsp;1.
 * 
 * Compartment also has an optional attribute "spatialDimensions", whose
 * value must be a positive integer indicating the number of spatial
 * dimensions possessed by the compartment.  The maximum value is @c 3,
 * meaning a three-dimensional structure (a volume).  Other permissible
 * values are @c 2 (for a two-dimensional area), @c 1 (for a
 * one-dimensional curve), and @c 0 (for a point).  The default value of
 * "spatialDimensions" is @c 3.
 *
 * Compartment has another optional attribute named "size", representing
 * the initial total size of the compartment.  The "size" attribute must be
 * a floating-point value and may represent a volume (if the compartment is
 * a three-dimensional one), or an area (if the compartment is
 * two-dimensional), or a length (if the compartment is one-dimensional).
 * There is no default value of compartment size in SBML Level 2.  In
 * particular, a missing "size" value <em>does not imply that the
 * compartment size is 1</em>.  (This is unlike the definition of
 * compartment "volume" in SBML Level 1.)  When the compartment's
 * "spatialDimensions" attribute does not have a value of @c 0, a missing
 * value of "size" for a given compartment signifies that the value either
 * is unknown, or to be obtained from an external source, or determined by
 * an InitialAssignment, AssignmentRule, AlgebraicRule or RateRule
 * elsewhere in the model.  The "size" attribute must not be present if the
 * "spatialDimensions" attribute has a value of @c 0; otherwise, a logical
 * inconsistency would exist because a zero-dimensional object cannot have
 * a physical size.
 *
 * The units associated with a compartment's "size" attribute value may be
 * set using the optional Compartment attribute "units".  The default
 * units, and the kinds of units allowed as values of the attribute
 * "units", interact with the number of spatial dimensions of the
 * compartment.  The value of the "units" attribute of a Compartment object
 * must be one of the base units (see Unit), or the predefined unit
 * identifiers @c volume, @c area, @c length or @c dimensionless, or a new
 * unit defined by a UnitDefinition object in the enclosing Model, subject
 * to the restrictions detailed in the following table:
 *
 * @htmlinclude libsbml-compartment-size-restrictions.html 
 *
 * In SBML Level 2, the units of the compartment size, as defined by the
 * "units" attribute or (if "units" is not set) the default value listed in
 * the table above, are used in the following ways when the compartment has
 * a "spatialDimensions" value greater than @c 0:
 * <ul>
 * <li> The value of the "units" attribute is used as the units of the
 * compartment identifier when the identifier appears as a numerical
 * quantity in a mathematical formula expressed in MathML.
 * 
 * <li> The @c math element of an AssignmentRule or InitialAssignment
 * referring to this compartment must have identical units.
 *
 * <li> In RateRule objects that set the rate of change of the compartment's
 * size, the units of the rule's @c math element must be identical to the
 * compartment's "units" attribute divided by the default @em time units.
 * (In other words, the units for the rate of change of compartment size
 * are <em>compartment size</em>/<em>time</em> units.
 *
 * <li> When a Species is to be treated in terms of concentrations or
 * density, the units of the spatial size portion of the concentration
 * value (i.e., the denominator in the units formula @em substance/@em
 * size) are those indicated by the value of the "units" attribute on the
 * compartment in which the species is located.
 * </ul>
 *
 * Compartments with "spatialDimensions"=@c 0 require special treatment in
 * this framework.  If a compartment has no size or dimensional units, how
 * should such a compartment's identifier be interpreted when it appears in
 * mathematical formulas?  The answer is that such a compartment's
 * identifier should not appear in mathematical formulas in the first
 * place&mdash;it has no value, and its value cannot change.  Note also
 * that a zero-dimensional compartment is a point, and species located at
 * points can only be described in terms of amounts, not
 * spatially-dependent measures such as concentration.  Since SBML
 * KineticLaw formulas are already in terms of @em substance/@em time and
 * not (say) @em concentration/@em time, volume or other factors in
 * principle are not needed for species located in zero-dimensional
 * compartments.
 *
 * Compartment has another optional attribute named "constant".  This takes
 * a boolean value indicating whether the compartment's size stays constant
 * or can vary during a simulation.  A value of @c false indicates the
 * compartment's "size" can be changed by other constructs in SBML.  A
 * value of @c true indicates the compartment's "size" cannot be changed by
 * any other construct except InitialAssignment.  In the special case of
 * "spatialDimensions"=@c 0, the value cannot be changed by
 * InitialAssignment either.  The default value for the "constant"
 * attribute is @c true because in the most common modeling scenarios at
 * the time of this writing, compartment sizes remain constant.  The
 * "constant" attribute must default to or be set to @c true if the value
 * of the "spatialDimensions" attribute is @c 0, because a zero-dimensional
 * compartment cannot ever have a size.
 *
 * Finally, Compartment has an optional attribute named "outside", whose
 * value can be the identifier of another Compartment object defined in the
 * enclosing Model object.  Doing so means that the other compartment
 * contains it or is outside of it.  This enables the representation of
 * simple topological relationships between compartments, for those
 * simulation systems that can make use of the information (e.g., for
 * drawing simple diagrams of compartments).  There are two restrictions on
 * the containment relationships in SBML.  First, because a compartment
 * with "spatialDimensions" of @c 0 has no size, such a compartment cannot
 * act as the container of any other compartment @em except compartments
 * that @em also have "spatialDimensions" values of @c 0.  Second, the
 * directed graph formed by representing Compartment structures as vertexes
 * and the "outside" attribute values as edges must be acyclic.  The latter
 * condition is imposed to prevent a compartment from being contained
 * inside itself.  In the absence of a value for "outside", compartment
 * definitions in SBML Level 2 do not have any implied spatial
 * relationships between each other.
 *
 * It is worth noting that in SBML, there is no relationship between
 * compartment sizes when compartment positioning is expressed using the
 * "outside" attribute.  The size of a given compartment does not in any
 * sense include the sizes of other compartments having it as the value of
 * their "outside" attributes.  In other words, if a compartment @em B has
 * the identifier of compartment @em A as its "outside" attribute value,
 * the size of @em A does not include the size of @em B.  The compartment
 * sizes are separate.
 * 
 * @note the "size" attribute on a compartment must be defined as optional;
 * however, <em>it is extremely good practice to specify values for
 * compartment sizes</em> when such values are available.  There are three
 * major technical reasons for this.  First, if the model contains any
 * species whose initial amounts are given in terms of concentrations, and
 * there is at least one reaction in the model referencing such a species,
 * then the model is numerically incomplete if it lacks a value for the
 * size of the compartment in which the species is located.  The reason is
 * simply that SBML Reaction objects defined in units of
 * <em>substance</em>/<em>time</em>, not concentration per time, and
 * thus the compartment size must at some point be used to convert from
 * species concentration to substance units.  Second, models ideally should
 * be instantiable in a variety of simulation frameworks.  A commonly-used
 * one is the discrete stochastic framework in which species are
 * represented as item counts (e.g., molecule counts).  If species' initial
 * quantities are given in terms of concentrations or densities, it is
 * impossible to convert the values to item counts without knowing
 * compartment sizes.  Third, if a model contains multiple compartments
 * whose sizes are not all identical to each other, it is impossible to
 * quantify the reaction rate expressions without knowing the compartment
 * volumes.  The reason for the latter is again that reaction rates in SBML
 * are defined in terms of <em>substance</em>/<em>time</em>, and when
 * species quantities are given in terms of concentrations or densities,
 * the compartment sizes become factors in the reaction rate expressions.
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class ListOfCompartments
 * @brief LibSBML implementation of SBML's %ListOfCompartments construct.
 * 
 * The various ListOf___ classes in SBML are merely containers used for
 * organizing the main components of an SBML model.  All are derived from
 * the abstract class SBase, and inherit the various attributes and
 * subelements of SBase, such as "metaid" as and "annotation".  The
 * ListOf___ classes do not add any attributes of their own.
 *
 * The relationship between the lists and the rest of an SBML model is
 * illustrated by the following (for SBML Level&nbsp;2 Version&nbsp;4):
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

#ifndef Compartment_h
#define Compartment_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/annotation/RDFAnnotation.h>
#include <sbml/common/operationReturnValues.h>

#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLVisitor;


class LIBSBML_EXTERN Compartment : public SBase
{
public:

  /**
   * Creates a new Compartment using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this Compartment
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Compartment
   * 
   * @note Upon the addition of a Compartment object to an SBMLDocument
   * (e.g., using Model::addCompartment()), the SBML Level, SBML Version
   * version and XML namespace of the document @em override the values used
   * when creating the Compartment object via this constructor.  This is
   * necessary to ensure that an SBML document is a consistent structure.
   * Nevertheless, the ability to supply the values at the time of creation
   * of a Compartment is an important aid to producing valid SBML.
   * Knowledge of the intented SBML Level and Version determine whether it
   * is valid to assign a particular value to an attribute, or whether it
   * is valid to add an object to an existing SBMLDocument.
   */
  Compartment (unsigned int level, unsigned int version);
  
  
  /**
   * Creates a new Compartment using the given SBMLNamespaces object 
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp;3 Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * It is worth emphasizing that although this constructor does not take
   * an identifier argument, in SBML Level&nbsp;2 and beyond, the "id"
   * (identifier) attribute of a Compartment is required to have a value.
   * Thus, callers are cautioned to assign a value after calling this
   * constructor.  Setting the identifier can be accomplished using the
   * method @if clike SBase::setId() @endif@if java SBase::setId(String id) @endif.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Upon the addition of a Compartment object to an SBMLDocument
   * (e.g., using Model::addCompartment()), the SBML XML namespace of the
   * document @em overrides the value used when creating the Compartment
   * object via this constructor.  This is necessary to ensure that an SBML
   * document is a consistent structure.  Nevertheless, the ability to
   * supply the values at the time of creation of a Compartment is an
   * important aid to producing valid SBML.  Knowledge of the intented SBML
   * Level and Version determine whether it is valid to assign a particular
   * value to an attribute, or whether it is valid to add an object to an
   * existing SBMLDocument.
   */
  Compartment (SBMLNamespaces* sbmlns);


  /**
   * Destroys this Compartment.
   */
  virtual ~Compartment ();


  /**
   * Copy constructor; creates a copy of a Compartment.
   * 
   * @param orig the Compartment instance to copy.
   */
  Compartment(const Compartment& orig);


  /**
   * Assignment operator for Compartment.
   */
  Compartment& operator=(const Compartment& orig);


  /**
   * Accepts the given SBMLVisitor for this instance of Compartment.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next Compartment in the
   * list of compartments within which this Compartment is embedded (i.e.,
   * the ListOfCompartments in the parent Model).
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this Compartment.
   * 
   * @return a (deep) copy of this Compartment.
   */
  virtual Compartment* clone () const;


  /**
   * Initializes the fields of this Compartment object to "typical" default
   * values.
   *
   * The SBML Compartment component has slightly different aspects and
   * default attribute values in different SBML Levels and Versions.
   * This method sets the values to certain common defaults, based
   * mostly on what they are in SBML Level&nbsp;2.  Specifically:
   * <ul>
   * <li> Sets attribute "spatialDimensions" to @c 3
   * <li> Sets attribute "constant" to @c true
   * <li> (Applies to Level&nbsp;1 models only) Sets attribute "volume" to @c 1.0
   * </ul>
   */
  void initDefaults ();


  /**
   * Returns the value of the "id" attribute of this Compartment.
   * 
   * @return the id of this Compartment.
   */
  const std::string& getId () const;


  /**
   * Returns the value of the "name" attribute of this Compartment.
   * 
   * @return the name of this Compartment.
   */
  const std::string& getName () const;


  /**
   * Get the compartment type of this Compartment, as indicated by the
   * Compartment object's "compartmentType" attribute value.
   * 
   * @return the value of the "compartmentType" attribute of this
   * Compartment as a string.
   */
  const std::string& getCompartmentType () const;


  /**
   * Get the number of spatial dimensions of this Compartment object.
   * 
   * @return the value of the "spatialDimensions" attribute of this
   * Compartment as an unsigned integer
   */
  unsigned int getSpatialDimensions () const;


  /**
   * Get the number of spatial dimensions of this Compartment object
   * as a double.
   *
   * @note SBML L3 changed the type of the spatialDimensions attribute
   * to double. To avoid backward compatibility issues this new function
   * was implemented.
   *
   * @return the value of the "spatialDimensions" attribute of this
   * Compartment as a double or NaN if the model is not SBML L3
   */
  double getSpatialDimensionsAsDouble () const;


  /**
   * Get the size of this Compartment.
   *
   * This method is identical to getVolume().  In SBML Level 1,
   * compartments are always three-dimensional constructs and only have
   * volumes, whereas in SBML Level 2, compartments may be other than
   * three-dimensional and therefore the "volume" attribute is named "size"
   * in Level 2.  LibSBML provides both getSize() and getVolume() for
   * easier compatibility between SBML Levels.
   *
   * @return the value of the "size" attribute ("volume" in Level 1) of
   * this Compartment as a float-point number.
   *
   * @see isSetSize()
   */
  double getSize () const;


  /**
   * (For SBML Level 1) Get the volume of this Compartment.
   * 
   * This method is identical to getSize().  In SBML Level 1, compartments
   * are always three-dimensional constructs and only have volumes, whereas
   * in SBML Level 2, compartments may be other than three-dimensional and
   * therefore the "volume" attribute is named "size" in Level 2.  LibSBML
   * provides both getSize() and getVolume() for easier compatibility
   * between SBML Levels.
   *
   * @return the value of the "volume" attribute ("size" in Level 2) of
   * this Compartment, as a floating-point number.
   *
   * @see isSetVolume()
   */
  double getVolume () const;


  /**
   * Get the units of this compartment's size.
   * 
   * The value of an SBML compartment's "units" attribute establishes the
   * unit of measurement associated with the compartment's size.
   *
   * @return the value of the "units" attribute of this Compartment, as a
   * string.  An empty string indicates that no units have been assigned to
   * the value of the size.
   *
   * @note @htmlinclude unassigned-units-are-not-a-default.html
   *
   * @see isSetUnits()
   * @see setUnits()
   * @see getSize()
   */
  const std::string& getUnits () const;


  /**
   * Get the identifier, if any, of the compartment that is designated
   * as being outside of this one.
   * 
   * @return the value of the "outside" attribute of this Compartment.
   */
  const std::string& getOutside () const;


  /**
   * Get the value of the "constant" attribute of this Compartment.
   *
   * @return @c true if this Compartment's size is flagged as being
   * constant, @c false otherwise.
   */
  bool getConstant () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "id" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "id" attribute of this Compartment has been
   * set, @c false otherwise.
   */
  bool isSetId () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "name" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "name" attribute of this Compartment has been
   * set, @c false otherwise.
   */
  bool isSetName () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "compartmentType" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return @c true if the "compartmentType" attribute of this Compartment
   * has been set, @c false otherwise.
   */
  bool isSetCompartmentType () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "size" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * This method is similar but not identical to isSetVolume().  The latter
   * should be used in the context of SBML Level 1 models instead of
   * isSetSize() because isSetVolume() performs extra processing to take
   * into account the difference in default values between SBML Levels 1
   * and 2.
   * 
   * @return @c true if the "size" attribute ("volume" in Level) of this
   * Compartment has been set, @c false otherwise.
   *
   * @see isSetVolume()
   * @see setSize(double value)
   */
  bool isSetSize () const;


  /**
   * (For SBML Level 1) Predicate returning @c true if this Compartment's
   * "volume" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * This method is similar but not identical to isSetSize().  The latter
   * should not be used in the context of SBML Level 1 models because this
   * method (isSetVolume()) performs extra processing to take into account
   * the difference in default values between SBML Levels 1 and 2.
   * 
   * @return @c true if the "volume" attribute ("size" in L2) of this
   * Compartment has been set, @c false otherwise.
   *
   * @see isSetSize()
   * @see setVolume(double value)
   *
   * @note In SBML Level 1, a compartment's volume has a default value (@c
   * 1.0) and therefore this method will always return @c true.  In Level
   * 2, a compartment's size (the equivalent of SBML Level 1's "volume") is
   * optional and has no default value, and therefore may or may not be
   * set.
   */
  bool isSetVolume () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "units" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return @c true if the "units" attribute of this Compartment has been
   * set, @c false otherwise.
   *
   * @note @htmlinclude unassigned-units-are-not-a-default.html
   */
  bool isSetUnits () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "outside" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return @c true if the "outside" attribute of this Compartment has
   * been set, @c false otherwise.
   */
  bool isSetOutside () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "spatialDimensions" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "spatialDimensions" attribute of this Compartment has
   * been set, @c false otherwise.
   */
  bool isSetSpatialDimensions () const;


  /**
   * Predicate returning @c true if this
   * Compartment's "constant" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "constant" attribute of this Compartment has
   * been set, @c false otherwise.
   */
  bool isSetConstant () const;


  /**
   * Sets the value of the "id" attribute of this Compartment.
   *
   * The string @p sid is copied.  Note that SBML has strict requirements
   * for the syntax of identifiers.  @htmlinclude id-syntax.html
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param sid the string to use as the identifier of this Compartment
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setId (const std::string& sid);


  /**
   * Sets the value of the "name" attribute of this Compartment.
   *
   * The string in @p name is copied.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param name the new name for the Compartment
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setName (const std::string& name);


  /**
   * Sets the "compartmentType" attribute of this Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param sid the identifier of a CompartmentType object defined
   * elsewhere in this Model.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int setCompartmentType (const std::string& sid);


  /**
   * Sets the "spatialDimensions" attribute of this Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * If @p value is not one of @c 0, @c 1, @c 2, or @c 3, this method will
   * have no effect (i.e., the "spatialDimensions" attribute will not be
   * set).
   * 
   * @param value an unsigned integer indicating the number of dimensions
   * of this compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int setSpatialDimensions (unsigned int value);


  /**
   * Sets the "spatialDimensions" attribute of this Compartment as a double.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value a double indicating the number of dimensions
   * of this compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int setSpatialDimensions (double value);


  /**
   * Sets the "size" attribute (or "volume" in SBML Level 1) of this
   * Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * This method is identical to setVolume() and is provided for
   * compatibility between SBML Level 1 and Level 2.
   *
   * @param value a @c double representing the size of this compartment
   * instance in whatever units are in effect for the compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int setSize (double value);


  /**
   * Sets the "volume" attribute (or "size" in SBML Level 2) of this
   * Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * This method is identical to setVolume() and is provided for
   * compatibility between SBML Level 1 and Level 2.
   * 
   * @param value a @c double representing the volume of this compartment
   * instance in whatever units are in effect for the compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int setVolume (double value);


  /**
   * Sets the "units" attribute of this Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param sid the identifier of the defined units to use.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setUnits (const std::string& sid);


  /**
   * Sets the "outside" attribute of this Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param sid the identifier of a compartment that encloses this one.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setOutside (const std::string& sid);


  /**
   * Sets the value of the "constant" attribute of this Compartment.
   *
   * @param value a boolean indicating whether the size/volume of this
   * compartment should be considered constant (@c true) or variable (@c
   * false)
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int setConstant (bool value);


  /**
   * Unsets the value of the "name" attribute of this Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetName ();


  /**
   * Unsets the value of the "compartmentType" attribute of this
   * Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @see setCompartmentType(const std::string& sid)
   * @see isSetCompartmentType()
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetCompartmentType ();


  /**
   * Unsets the value of the "size" attribute of this Compartment.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int unsetSize ();


  /**
   * (For SBML Level 1) Unsets the value of the "volume" attribute of this
   * Compartment.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * In SBML Level 1, a Compartment volume has a default value (1.0) and
   * therefore <em>should always be set</em>.  In Level 2, "size" is
   * optional with no default value and as such may or may not be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int unsetVolume ();


  /**
   * Unsets the value of the "units" attribute of this Compartment.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetUnits ();


  /**
   * Unsets the value of the "outside" attribute of this Compartment.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetOutside ();


  /**
   * Unsets the value of the "spatialDimensions" attribute of this Compartment.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @note This function is only valid for SBML L3
   */
  int unsetSpatialDimensions ();


  /**
   * Constructs and returns a UnitDefinition that corresponds to the units
   * of this Compartment's designated size.
   *
   * Compartments in SBML have an attribute ("units") for declaring the
   * units of measurement intended for the value of the compartment's size.
   * In the absence of a value given for this attribute, the units are
   * taken from the model's definition of @c "volume", @c "area", @c
   * "length" units, depending on the value given to this Compartment's
   * "size" attribute, or (if the Model does not redefine them) the
   * corresponding SBML default units for those quantities.  Following that
   * procedure, the method getDerivedUnitDefinition() returns a
   * UnitDefinition based on the interpreted units of this compartment's
   * size.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return NULL.
   *
   * Note also that unit declarations for Compartment are in terms of the
   * @em identifier of a unit, but this method returns a UnitDefinition
   * object, not a unit identifier.  It does this by constructing an
   * appropriate UnitDefinition.  Callers may find this particularly useful
   * when used in conjunction with the helper methods on UnitDefinition for
   * comparing different UnitDefinition objects.
   * 
   * @return a UnitDefinition that expresses the units of this 
   * Compartment.
   *
   * @see getUnits()
   */
  UnitDefinition * getDerivedUnitDefinition();


  /**
   * Constructs and returns a UnitDefinition that corresponds to the units
   * of this Compartment's designated size.
   *
   * Compartments in SBML have an attribute ("units") for declaring the
   * units of measurement intended for the value of the compartment's size.
   * In the absence of a value given for this attribute, the units are
   * taken from the model's definition of @c "volume", @c "area", @c
   * "length" units, depending on the value given to this Compartment's
   * "size" attribute, or (if the Model does not redefine them) the
   * corresponding SBML default units for those quantities.  Following that
   * procedure, the method getDerivedUnitDefinition() returns a
   * UnitDefinition based on the interpreted units of this compartment's
   * size.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return NULL.
   *
   * Note also that unit declarations for Compartment are in terms of the
   * @em identifier of a unit, but this method returns a UnitDefinition
   * object, not a unit identifier.  It does this by constructing an
   * appropriate UnitDefinition.  Callers may find this particularly useful
   * when used in conjunction with the helper methods on UnitDefinition for
   * comparing different UnitDefinition objects.
   * 
   * @return a UnitDefinition that expresses the units of this 
   * Compartment.
   *
   * @see getUnits()
   */
  const UnitDefinition * getDerivedUnitDefinition() const;


  /**
   * Returns the libSBML type code for this SBML object.
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
   * @return the SBML type code for this object, or @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Returns the XML element name of this object, which for Compartment, is
   * always @c "compartment".
   * 
   * @return the name of this element, i.e., @c "compartment".
   */
  virtual const std::string& getElementName () const;


  /**
   * Predicate returning @c true if
   * all the required attributes for this Compartment object
   * have been set.
   *
   * @note The required attributes for a Compartment object are:
   * @li id (name in L1)
   * @li constant (in L3 only)
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


protected:

  /** @cond doxygen-libsbml-internal */

  /**
   * This is a constructor that takes no arguments and 
   * only exists because the validator code needs it.
   */
  Compartment ();

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  void readL1Attributes (const XMLAttributes& attributes);

  void readL2Attributes (const XMLAttributes& attributes);
  
  void readL3Attributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  std::string   mId;
  std::string   mName;
  std::string   mCompartmentType;
  unsigned int  mSpatialDimensions;
  double        mSpatialDimensionsDouble;
  double        mSize;
  std::string   mUnits;
  std::string   mOutside;
  bool          mConstant;

  bool  mIsSetSize;
  bool  mIsSetSpatialDimensions;
  bool  mIsSetConstant;

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
  friend class L3v1CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;

  /** @endcond */
};


class LIBSBML_EXTERN ListOfCompartments : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfCompartments instance.
   *
   * @return a (deep) copy of this ListOfCompartments.
   */
  virtual ListOfCompartments* clone () const;


  /**
   * Returns the libSBML type code for this SBML object.
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
   * @return the SBML type code for this object, or @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).

   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const { return SBML_LIST_OF; };


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., Compartment objects, if the list is non-empty).
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
   * instance, or @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;


  /**
   * Returns the XML element name of this object.
   *
   * For ListOfCompartments, the XML element name is @c "listOfCompartments".
   * 
   * @return the name of this element, i.e., @c "listOfCompartments".
   */
  virtual const std::string& getElementName () const;


  /**
   * Get a Compartment from the ListOfCompartments.
   *
   * @param n the index number of the Compartment to get.
   * 
   * @return the nth Compartment in this ListOfCompartments.
   *
   * @see size()
   */
  virtual Compartment * get(unsigned int n); 


  /**
   * Get a Compartment from the ListOfCompartments.
   *
   * @param n the index number of the Compartment to get.
   * 
   * @return the nth Compartment in this ListOfCompartments.
   *
   * @see size()
   */
  virtual const Compartment * get(unsigned int n) const; 

  /**
   * Get a Compartment from the ListOfCompartments
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Compartment to get.
   * 
   * @return Compartment in this ListOfCompartments
   * with the given id or NULL if no such
   * Compartment exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual Compartment* get (const std::string& sid);


  /**
   * Get a Compartment from the ListOfCompartments
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Compartment to get.
   * 
   * @return Compartment in this ListOfCompartments
   * with the given id or NULL if no such
   * Compartment exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const Compartment* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfCompartments items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   *
   * @see size()
   */
  virtual Compartment* remove (unsigned int n);


  /**
   * Removes item in this ListOfCompartments items with the given identifier.
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
  virtual Compartment* remove (const std::string& sid);


  /** @cond doxygen-libsbml-internal */

  /**
   * Get the ordinal position of this element in the containing object
   * (which in this case is the Model object).
   *
   * The ordering of elements in the XML form of SBML is generally fixed
   * for most components in SBML.  So, for example, the ListOfCompartments
   * in a model is (in SBML Level 2 Version 4) the fifth ListOf___.
   * (However, it differs for different Levels and Versions of SBML.)
   *
   * @return the ordinal position of the element with respect to its
   * siblings, or @c -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /** @endcond */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


/*
LIBSBML_EXTERN
Compartment_t *
Compartment_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version, XMLNamespaces_t *xmlns);
*/

LIBSBML_EXTERN
Compartment_t *
Compartment_create (unsigned int level, unsigned int version);


LIBSBML_EXTERN
Compartment_t *
Compartment_createWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
void
Compartment_free (Compartment_t *c);


LIBSBML_EXTERN
Compartment_t *
Compartment_clone (const Compartment_t* c);


LIBSBML_EXTERN
void
Compartment_initDefaults (Compartment_t *c);


LIBSBML_EXTERN
const XMLNamespaces_t *
Compartment_getNamespaces(Compartment_t *c);


LIBSBML_EXTERN
const char *
Compartment_getId (const Compartment_t *c);


LIBSBML_EXTERN
const char *
Compartment_getName (const Compartment_t *c);


LIBSBML_EXTERN
const char *
Compartment_getCompartmentType (const Compartment_t *c);


LIBSBML_EXTERN
unsigned int
Compartment_getSpatialDimensions (const Compartment_t *c);


LIBSBML_EXTERN
double
Compartment_getSpatialDimensionsAsDouble (const Compartment_t *c);


LIBSBML_EXTERN
double
Compartment_getSize (const Compartment_t *c);


LIBSBML_EXTERN
double
Compartment_getVolume (const Compartment_t *c);


LIBSBML_EXTERN
const char *
Compartment_getUnits (const Compartment_t *c);


LIBSBML_EXTERN
const char *
Compartment_getOutside (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_getConstant (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetId (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetName (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetCompartmentType (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetSize (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetVolume (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetUnits (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetOutside (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetSpatialDimensions (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_isSetConstant (const Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_setId (Compartment_t *c, const char *sid);


LIBSBML_EXTERN
int
Compartment_setName (Compartment_t *c, const char *string);


LIBSBML_EXTERN
int
Compartment_setCompartmentType (Compartment_t *c, const char *sid);


LIBSBML_EXTERN
int
Compartment_setSpatialDimensions (Compartment_t *c, unsigned int value);


LIBSBML_EXTERN
int
Compartment_setSpatialDimensionsAsDouble (Compartment_t *c, double value);


LIBSBML_EXTERN
int
Compartment_setSize (Compartment_t *c, double value);


LIBSBML_EXTERN
int
Compartment_setVolume (Compartment_t *c, double value);


LIBSBML_EXTERN
int
Compartment_setUnits (Compartment_t *c, const char *sid);


LIBSBML_EXTERN
int
Compartment_setOutside (Compartment_t *c, const char *sid);


LIBSBML_EXTERN
int
Compartment_setConstant (Compartment_t *c, int value);


LIBSBML_EXTERN
int
Compartment_unsetName (Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_unsetCompartmentType (Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_unsetSize (Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_unsetVolume (Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_unsetUnits (Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_unsetOutside (Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_unsetSpatialDimensions (Compartment_t *c);


LIBSBML_EXTERN
UnitDefinition_t * 
Compartment_getDerivedUnitDefinition(Compartment_t *c);


LIBSBML_EXTERN
int
Compartment_hasRequiredAttributes (Compartment_t *c);


LIBSBML_EXTERN
Compartment_t *
ListOfCompartments_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
Compartment_t *
ListOfCompartments_removeById (ListOf_t *lo, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* Compartment_h */
