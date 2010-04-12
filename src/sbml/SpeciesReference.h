/**
 * @file    SpeciesReference.h
 * @brief   Definitions of SimpleSpeciesReference, SpeciesReference,
 *          ModifierSpeciesReference, and ListOfSpeciesReferences. 
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
 * @class SimpleSpeciesReference
 * @brief LibSBML implementation of %SBML's %SimpleSpeciesReference construct.
 *
 * As mentioned in the description of Reaction, every species that enters
 * into a given reaction must appear in that reaction's lists of reactants,
 * products and/or modifiers.  In an SBML model, all species that may
 * participate in any reaction are listed in the "listOfSpecies" element of
 * the top-level Model object.  Lists of products, reactants and modifiers
 * in Reaction objects do not introduce new species, but rather, they refer
 * back to those listed in the model's top-level "listOfSpecies".  For
 * reactants and products, the connection is made using SpeciesReference
 * objects; for modifiers, it is made using ModifierSpeciesReference
 * objects.  SimpleSpeciesReference is an abstract type that serves as the
 * parent class of both SpeciesReference and ModifierSpeciesReference.  It
 * is used simply to hold the attributes and elements that are common to
 * the latter two structures.
 *
 * The SimpleSpeciesReference structure has a mandatory attribute,
 * "species", which must be a text string conforming to the identifer
 * syntax permitted in %SBML.  This attribute is inherited by the
 * SpeciesReference and ModifierSpeciesReference subclasses derived from
 * SimpleSpeciesReference.  The value of the "species" attribute must be
 * the identifier of a species defined in the enclosing Model.  The species
 * is thereby declared as participating in the reaction being defined.  The
 * precise role of that species as a reactant, product, or modifier in the
 * reaction is determined by the subclass of SimpleSpeciesReference (i.e.,
 * either SpeciesReference or ModifierSpeciesReference) in which the
 * identifier appears.
 * 
 * SimpleSpeciesReference also contains an optional attribute, "id",
 * allowing instances to be referenced from other structures.  No SBML
 * structures currently do this; however, such structures are anticipated
 * in future SBML Levels.
 *
 * 
 * <!---------------------------------------------------------------------- -->
 *
 * @class SpeciesReference
 * @brief LibSBML implementation of %SBML's %SpeciesReference construct.
 *
 * The Reaction structure provides a way to express which species act as
 * reactants and which species act as products in a reaction.  In a given
 * reaction, references to those species acting as reactants and/or
 * products are made using instances of SpeciesReference structures in a
 * Reaction object's lists of reactants and products.
 *
 * The mandatory "species" attribute of SpeciesReference must have as its
 * value the identifier of an existing species defined in the enclosing
 * Model.  The species is thereby designated as a reactant or product in
 * the reaction.  Which one it is (i.e., reactant or product) is indicated
 * by whether the SpeciesReference appears in the Reaction's "reactant" or
 * "product" lists.
 * 
 * Product and reactant stoichiometries can be specified using
 * <em>either</em> "stoichiometry" or "stoichiometryMath" in a
 * SpeciesReference object.  The "stoichiometry" attribute is of type
 * double and should contain values greater than zero (0).  The
 * "stoichiometryMath" element is implemented as an element containing a
 * MathML expression.  These two are mutually exclusive; only one of
 * "stoichiometry" or "stoichiometryMath" should be defined in a given
 * SpeciesReference instance.  When neither the attribute nor the element
 * is present, the value of "stoichiometry" in the SpeciesReference
 * instance defaults to @c 1.
 *
 * For maximum interoperability, the "stoichiometry" attribute should be
 * used in preference to "stoichiometryMath" when a species' stoichiometry
 * is a simple scalar number (integer or decimal).  When the stoichiometry
 * is a rational number, or when it is a more complicated formula,
 * "stoichiometryMath" must be used.  The MathML expression in
 * "stoichiometryMath" may also refer to identifiers of entities in a model
 * (except reaction identifiers).  However, the only species identifiers
 * that can be used in "stoichiometryMath" are those referenced in the
 * Reaction list of reactants, products and modifiers.
 *
 * The following is a simple example of a species reference for species @c
 * X0, with stoichiometry @c 2, in a list of reactants within a reaction
 * having the identifier @c J1:
 * @code
 * <model>
 *     ...
 *     <listOfReactions>
 *         <reaction id="J1">
 *             <listOfReactants>
 *                 <speciesReference species="X0" stoichiometry="2">
 *             </listOfReactants>
 *             ...
 *         </reaction>
 *         ...
 *     </listOfReactions>
 *     ...
 * </model>
 * @endcode
 *
 * The following is a more complex example of a species reference for
 * species X0, with a stoichiometry formula consisting of the parameter
 * @c x:
 * @code
 * <model>
 *     ...
 *     <listOfReactions>
 *         <reaction id="J1">
 *             <listOfReactants>
 *                 <speciesReference species="X0">
 *                     <stoichiometryMath>
 *                         <math xmlns="http://www.w3.org/1998/Math/MathML">
 *                             <ci>x</ci>
 *                         </math>
 *                     </stoichiometryMath>
 *                 </speciesReference>
 *             </listOfReactants>
 *             ...
 *         </reaction>
 *         ...
 *     </listOfReactions>
 *     ...
 * </model>
 * @endcode
 *
 * A species can occur more than once in the lists of reactants and
 * products of a given Reaction instance.  The effective stoichiometry for
 * a species in a reaction is the sum of the stoichiometry values given on
 * the SpeciesReference object in the list of products minus the sum of
 * stoichiometry values given on the SpeciesReference objects in the list
 * of reactants.  A positive value indicates the species is effectively a
 * product and a negative value indicates the species is effectively a
 * reactant.  SBML places no restrictions on the effective stoichiometry of
 * a species in a reaction; for example, it can be zero.  In the following
 * SBML fragment, the two reactions have the same effective stoichiometry
 * for all their species:
 * @code
 * <reaction id="x">
 *     <listOfReactants>
 *         <speciesReference species="a"/>
 *         <speciesReference species="a"/>
 *         <speciesReference species="b"/>
 *     </listOfReactants>
 *     <listOfProducts>
 *         <speciesReference species="c"/>
 *         <speciesReference species="b"/>
 *     </listProducts>
 * </reaction>
 * <reaction id="y">
 *     <listOfReactants>
 *         <speciesReference species="a" stoichiometry="2"/>
 *     </listOfReactants>
 *     <listOfProducts>
 *         <speciesReference species="c"/>
 *     </listProducts>
 * </reaction>
 * @endcode
 *
 *
 * <!---------------------------------------------------------------------- -->
 *
 * @class ModifierSpeciesReference
 * @brief LibSBML implementation of %SBML's %ModifierSpeciesReference construct.
 *
 * Sometimes a species appears in the kinetic rate formula of a reaction
 * but is itself neither created nor destroyed in that reaction (for
 * example, because it acts as a catalyst or inhibitor).  In SBML, all such
 * species are simply called @em modifiers without regard to the detailed
 * role of those species in the model.  The Reaction structure provides a
 * way to express which species act as modifiers in a given reaction.  This
 * is the purpose of the list of modifiers available in Reaction.  The list
 * contains instances of ModifierSpeciesReference structures.
 *
 * The ModifierSpeciesReference structure inherits the mandatory attribute
 * "species" and optional attributes "id" and "name" from the parent class
 * SimpleSpeciesReference.  See the description of SimpleSpeciesReference
 * for more information about these.
 *
 * The value of the "species" attribute must be the identifier of a species
 * defined in the enclosing Model; this species is designated as a modifier
 * for the current reaction.  A reaction may have any number of modifiers.
 * It is permissible for a modifier species to appear simultaneously in the
 * list of reactants and products of the same reaction where it is
 * designated as a modifier, as well as to appear in the list of reactants,
 * products and modifiers of other reactions in the model.
 *
 * 
 * <!---------------------------------------------------------------------- -->
 *
 * @class ListOfSpeciesReferences
 * @brief LibSBML implementation of SBML's %ListOfSpeciesReferences construct.
 *
 * The ListOfSpeciesReferences class is used to store lists of reactants
 * and products in a Reaction object.
 *
 * As with the various other ListOf___ classes in %SBML, the
 * ListOfSpeciesReferences is merely a container used for organizing
 * instances of other objects, in this case SpeciesReference objects.
 * ListOfSpeciesReferences is derived from the abstract class SBase, and
 * inherit the various attributes and subelements of SBase, such as
 * "metaid" as and "annotation".  The ListOf___ classes do not add any
 * attributes of their own.
 */


#ifndef SpeciesReference_h
#define SpeciesReference_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/StoichiometryMath.h>
#include <sbml/ListOf.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class StoichiometryMath;
class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN SimpleSpeciesReference : public SBase
{
public:

  /**
   * Creates a new SimpleSpeciesReference using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this SimpleSpeciesReference
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * SimpleSpeciesReference
   * 
   * @note Once a SimpleSpeciesReference has been added to an SBMLDocument, the @p level,
   * @p version for the document @em override those used
   * to create the SimpleSpeciesReference.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  SimpleSpeciesReference (unsigned int level, unsigned int version);


  /**
   * Creates a new SimpleSpeciesReference using the given SBMLNamespaces object
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
   * @note Once a SimpleSpeciesReference has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the SimpleSpeciesReference.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  SimpleSpeciesReference (SBMLNamespaces* sbmlns);


  /**
   * Destroys this SimpleSpeciesReference.
   */
  virtual ~SimpleSpeciesReference ();


  /**
  * Copy constructor; creates a copy of this SimpleSpeciesReference.
  */
  SimpleSpeciesReference(const SimpleSpeciesReference& orig);


  /**
   * Assignment operator. 
   */
  SimpleSpeciesReference& operator=(const SimpleSpeciesReference& orig);


  /**
   * Accepts the given SBMLVisitor.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Returns the value of the "id" attribute of this SimpleSpeciesReference.
   * 
   * @return the id of this SimpleSpeciesReference.
   */
  const std::string& getId () const;


  /**
   * Returns the value of the "name" attribute of this SimpleSpeciesReference.
   * 
   * @return the name of this SimpleSpeciesReference.
   */
  const std::string& getName () const;


  /**
   * Get the value of the "species" attribute.
   * 
   * @return the value of the attribute "species" for this
   * SimpleSpeciesReference.
   */
  const std::string& getSpecies () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SimpleSpeciesReference's "id" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "id" attribute of this SimpleSpeciesReference has been
   * set, @c false otherwise.
   */
  bool isSetId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SimpleSpeciesReference's "name" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "name" attribute of this SimpleSpeciesReference has been
   * set, @c false otherwise.
   */
  bool isSetName () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SimpleSpeciesReference's "species" attribute has been set.
   * 
   * @return @c true if the "species" attribute of this
   * SimpleSpeciesReference has been set, @c false otherwise.
   */
  bool isSetSpecies () const;


  /**
   * Sets the "species" attribute of this SimpleSpeciesReference.
   *
   * The identifier string passed in @p sid is copied.
   *
   * @param sid the identifier of a species defined in the enclosing
   * Model's ListOfSpecies.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setSpecies (const std::string& sid);


  /**
   * Sets the value of the "id" attribute of this SimpleSpeciesReference.
   *
   * The string @p sid is copied.  Note that SBML has strict requirements
   * for the syntax of identifiers.  The following is summary of the
   * definition of the SBML identifier type @c SId (here expressed in an
   * extended form of BNF notation):
   * @code
   *   letter ::= 'a'..'z','A'..'Z'
   *   digit  ::= '0'..'9'
   *   idChar ::= letter | digit | '_'
   *   SId    ::= ( letter | '_' ) idChar*
   * @endcode
   * The equality of SBML identifiers is determined by an exact character
   * sequence match; i.e., comparisons must be performed in a
   * case-sensitive manner.  In addition, there are a few conditions for
   * the uniqueness of identifiers in an SBML model.  Please consult the
   * SBML specifications for the exact formulations.
   *
   * @param sid the string to use as the identifier of this SimpleSpeciesReference
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setId (const std::string& sid);


  /**
   * Sets the value of the "name" attribute of this SimpleSpeciesReference.
   *
   * The string in @p name is copied.
   *
   * @param name the new name for the SimpleSpeciesReference
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setName (const std::string& name);


  /**
   * Unsets the value of the "id" attribute of this SimpleSpeciesReference.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  int unsetId ();


  /**
   * Unsets the value of the "name" attribute of this SimpleSpeciesReference.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  int unsetName ();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * is a ModifierSpeciesReference.
   * 
   * @return @c true if this SimpleSpeciesReference's subclass is
   * ModiferSpeciesReference, @c false if it is a plain SpeciesReference.
   */
  bool isModifier () const;



protected:
  /** @cond doxygen-libsbml-internal */

  virtual bool hasRequiredAttributes() const ;

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  SimpleSpeciesReference ();


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

  std::string  mId;
  std::string  mName;
  std::string  mSpecies;

  /** @endcond doxygen-libsbml-internal */
};



class LIBSBML_EXTERN SpeciesReference : public SimpleSpeciesReference
{
public:

  /**
   * Creates a new SpeciesReference using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this SpeciesReference
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * SpeciesReference
   * 
   * @note Once a SpeciesReference has been added to an SBMLDocument, the @p level,
   * @p version for the document @em override those used
   * to create the SpeciesReference.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  SpeciesReference (unsigned int level, unsigned int version);


  /**
   * Creates a new SpeciesReference using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Once a SpeciesReference has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the SpeciesReference.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  SpeciesReference (SBMLNamespaces* sbmlns);


  /**
   * Destroys this SpeciesReference.
   */
  virtual ~SpeciesReference ();


  /**
   * Copy constructor; creates a copy of this SpeciesReference.
   */
  SpeciesReference (const SpeciesReference& orig);


   /**
   * Assignment operator
   */
  SpeciesReference& operator=(const SpeciesReference& rhs);


  /**
   * Accepts the given SBMLVisitor.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this SpeciesReference instance.
   *
   * @return a (deep) copy of this SpeciesReference.
   */
  virtual SpeciesReference* clone () const;


  /**
   * Initializes the attributes of this SpeciesReference to their defaults.
   * <ul>
   * <li> stoichiometry is set to @c 1
   * <li> denominator is set to @c 1
   * </ul>
   *
   * @see getDenominator()
   * @see setDenominator(int value)
   * @see getStoichiometry()
   * @see setStoichiometry(double value)
   * @see getStoichiometryMath()
   * @see setStoichiometryMath(const StoichiometryMath* math)
   */
  void initDefaults ();


  /**
   * Get the value of the "stoichiometry" attribute.
   *
   * In SBML Level 2, Product and reactant stoichiometries can be specified
   * using <em>either</em> "stoichiometry" or "stoichiometryMath" in a
   * SpeciesReference object.  The former is to be used when a
   * stoichiometry is simply a scalar number, while the latter is for
   * occasions when it needs to be a rational number or it needs to
   * reference other mathematical expressions.  The "stoichiometry"
   * attribute is of type double and should contain values greater than
   * zero (0).  The "stoichiometryMath" element is implemented as an
   * element containing a MathML expression.  These two are mutually
   * exclusive; only one of "stoichiometry" or "stoichiometryMath" should
   * be defined in a given SpeciesReference instance.  When neither the
   * attribute nor the element is present, the value of "stoichiometry" in
   * the SpeciesReference instance defaults to @c 1.  For maximum
   * interoperability between different software tools, the "stoichiometry"
   * attribute should be * used in preference to "stoichiometryMath" when a
   * species' stoichiometry * is a simple scalar number (integer or
   * decimal).
   * 
   * @return the value of the (scalar) "stoichiometry" attribute of this
   * SpeciesReference.
   *
   * @see getStoichiometryMath()
   */
  double getStoichiometry () const;


  /**
   * Get the content of the "stoichiometryMath" subelement as an ASTNode
   * tree.
   *
   * In SBML Level 2, Product and reactant stoichiometries can be specified
   * using <em>either</em> "stoichiometry" or "stoichiometryMath" in a
   * SpeciesReference object.  The former is to be used when a
   * stoichiometry is simply a scalar number, while the latter is for
   * occasions when it needs to be a rational number or it needs to
   * reference other mathematical expressions.  The "stoichiometry"
   * attribute is of type double and should contain values greater than
   * zero (0).  The "stoichiometryMath" element is implemented as an
   * element containing a MathML expression.  These two are mutually
   * exclusive; only one of "stoichiometry" or "stoichiometryMath" should
   * be defined in a given SpeciesReference instance.  When neither the
   * attribute nor the element is present, the value of "stoichiometry" in
   * the SpeciesReference instance defaults to @c 1.  For maximum
   * interoperability between different software tools, the "stoichiometry"
   * attribute should be used in preference to "stoichiometryMath" when a
   * species' stoichiometry is a simple scalar number (integer or
   * decimal).
   * 
   * @return the content of the "stoichiometryMath" subelement of this
   * SpeciesReference.
   */
  const StoichiometryMath* getStoichiometryMath () const;


  /**
   * Get the content of the "stoichiometryMath" subelement as an ASTNode
   * tree.
   *
   * In SBML Level 2, Product and reactant stoichiometries can be specified
   * using <em>either</em> "stoichiometry" or "stoichiometryMath" in a
   * SpeciesReference object.  The former is to be used when a
   * stoichiometry is simply a scalar number, while the latter is for
   * occasions when it needs to be a rational number or it needs to
   * reference other mathematical expressions.  The "stoichiometry"
   * attribute is of type double and should contain values greater than
   * zero (0).  The "stoichiometryMath" element is implemented as an
   * element containing a MathML expression.  These two are mutually
   * exclusive; only one of "stoichiometry" or "stoichiometryMath" should
   * be defined in a given SpeciesReference instance.  When neither the
   * attribute nor the element is present, the value of "stoichiometry" in
   * the SpeciesReference instance defaults to @c 1.  For maximum
   * interoperability between different software tools, the "stoichiometry"
   * attribute should be used in preference to "stoichiometryMath" when a
   * species' stoichiometry is a simple scalar number (integer or
   * decimal).
   * 
   * @return the content of the "stoichiometryMath" subelement of this
   * SpeciesReference.
   */
  StoichiometryMath* getStoichiometryMath ();


  /**
   * Get the value of the "denominator" attribute, for the case of a
   * rational-numbered stoichiometry or a model in SBML Level 1.
   *
   * The "denominator" attribute is only actually written out in the case
   * of an SBML Level 1 model.  In SBML Level 2, rational-number
   * stoichiometries are written as MathML elements in the
   * "stoichiometryMath" subelement.  However, as a convenience to users,
   * libSBML allows the creation and manipulation of rational-number
   * stoichiometries by supplying the numerator and denominator directly
   * rather than having to manually create an ASTNode structure.  LibSBML
   * will write out the appropriate constructs (either a combination of
   * "stoichiometry" and "denominator" in the case of SBML Level 1, or a
   * "stoichiometryMath" subelement in the case of SBML Level 2).
   * 
   * @return the value of the "denominator" attribute of this
   * SpeciesReference.
   */
  int getDenominator () const;


  /**
   * Get the value of the "constant" attribute.
   * 
   * @return the value of the "constant" attribute of this
   * SpeciesReference.
   */
  bool getConstant () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesReference's "stoichiometryMath" subelement has been set
   * 
   * @return @c true if the "stoichiometryMath" subelement of this
   * SpeciesReference has been set, @c false otherwise.
   */
  bool isSetStoichiometryMath () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesReference's "constant" attribute has been set
   * 
   * @return @c true if the "constant" attribute of this
   * SpeciesReference has been set, @c false otherwise.
   */
  bool isSetConstant () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesReference's "stoichiometry" attribute has been set.
   * 
   * @return @c true if the "stoichiometry" attribute of this
   * SpeciesReference has been set, @c false otherwise.
   */
  bool isSetStoichiometry () const;


  /**
   * Sets the value of the "stoichiometry" attribute of this
   * SpeciesReference.
   *
   * In SBML Level 2, Product and reactant stoichiometries can be specified
   * using <em>either</em> "stoichiometry" or "stoichiometryMath" in a
   * SpeciesReference object.  The former is to be used when a
   * stoichiometry is simply a scalar number, while the latter is for
   * occasions when it needs to be a rational number or it needs to
   * reference other mathematical expressions.  The "stoichiometry"
   * attribute is of type double and should contain values greater than
   * zero (0).  The "stoichiometryMath" element is implemented as an
   * element containing a MathML expression.  These two are mutually
   * exclusive; only one of "stoichiometry" or "stoichiometryMath" should
   * be defined in a given SpeciesReference instance.  When neither the
   * attribute nor the element is present, the value of "stoichiometry" in
   * the SpeciesReference instance defaults to @c 1.  For maximum
   * interoperability between different software tools, the "stoichiometry"
   * attribute should be used in preference to "stoichiometryMath" when a
   * species' stoichiometry is a simple scalar number (integer or
   * decimal).
   * 
   * @param value the new value of the "stoichiometry" attribute
   *
   * @note In SBML Level 2, the "stoichiometryMath" subelement of this 
   *       SpeciesReference object will be unset because the "stoichiometry" 
   *       attribute and the stoichiometryMath" subelement are mutually 
   *       exclusive.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int setStoichiometry (double value);


  /**
   * Sets the "stoichiometryMath" subelement of this SpeciesReference.
   *
   * The Abstract Syntax Tree in @p math is copied.
   *
   * In SBML Level 2, Product and reactant stoichiometries can be specified
   * using <em>either</em> "stoichiometry" or "stoichiometryMath" in a
   * SpeciesReference object.  The former is to be used when a
   * stoichiometry is simply a scalar number, while the latter is for
   * occasions when it needs to be a rational number or it needs to
   * reference other mathematical expressions.  The "stoichiometry"
   * attribute is of type double and should contain values greater than
   * zero (0).  The "stoichiometryMath" element is implemented as an
   * element containing a MathML expression.  These two are mutually
   * exclusive; only one of "stoichiometry" or "stoichiometryMath" should
   * be defined in a given SpeciesReference instance.  When neither the
   * attribute nor the element is present, the value of "stoichiometry" in
   * the SpeciesReference instance defaults to @c 1.  For maximum
   * interoperability between different software tools, the "stoichiometry"
   * attribute should be used in preference to "stoichiometryMath" when a
   * species' stoichiometry is a simple scalar number (integer or
   * decimal).
   * 
   * @param math the StoichiometryMath expression that is to be copied as the
   * content of the "stoichiometryMath" subelement.
   *
   * @note In SBML Level 2, the "stoichiometry" attribute of this 
   *       SpeciesReference object will be unset (isSetStoichiometry() will 
   *       return @c false although getStoichiometry() will return 1.0) if the 
   *       given math is not null because the "stoichiometry" attribute and the 
   *       stoichiometryMath" subelement are mutually exclusive.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   */
  int setStoichiometryMath (const StoichiometryMath* math);



  /**
   * Set the value of the "denominator" attribute, for the case of a
   * rational-numbered stoichiometry or a model in SBML Level 1.
   *
   * The "denominator" attribute is only actually written out in the case
   * of an SBML Level 1 model.  In SBML Level 2, rational-number
   * stoichiometries are written as MathML elements in the
   * "stoichiometryMath" subelement.  However, as a convenience to users,
   * libSBML allows the creation and manipulation of rational-number
   * stoichiometries by supplying the numerator and denominator directly
   * rather than having to manually create an ASTNode structure.  LibSBML
   * will write out the appropriate constructs (either a combination of
   * "stoichiometry" and "denominator" in the case of SBML Level 1, or a
   * "stoichiometryMath" subelement in the case of SBML Level 2).
   *
   * @param value the scalar value 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int setDenominator (int value);


  /**
   * Sets the "constant" attribute of this SpeciesReference to the given boolean
   * @p flag.
   *
   * @param flag a boolean, the value for the "constant" attribute of this
   * SpeciesReference instance
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setConstant (bool flag);


  /**
   * Unsets the "stoichiometryMath" subelement of this SpeciesReference.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * In SBML Level 2, Product and reactant stoichiometries can be specified
   * using <em>either</em> "stoichiometry" or "stoichiometryMath" in a
   * SpeciesReference object.  The former is to be used when a
   * stoichiometry is simply a scalar number, while the latter is for
   * occasions when it needs to be a rational number or it needs to
   * reference other mathematical expressions.  The "stoichiometry"
   * attribute is of type double and should contain values greater than
   * zero (0).  The "stoichiometryMath" element is implemented as an
   * element containing a MathML expression.  These two are mutually
   * exclusive; only one of "stoichiometry" or "stoichiometryMath" should
   * be defined in a given SpeciesReference instance.  When neither the
   * attribute nor the element is present, the value of "stoichiometry" in
   * the SpeciesReference instance defaults to @c 1.  For maximum
   * interoperability between different software tools, the "stoichiometry"
   * attribute should be used in preference to "stoichiometryMath" when a
   * species' stoichiometry is a simple scalar number (integer or
   * decimal).
   *
   * @note In SBML Level 2, the "stoichiometry" attribute of this 
   *       SpeciesReference object will be reset to default (1.0) if the
   *       "stoichiometry" attribute has not been set.
   * 
   */
  int unsetStoichiometryMath ();


  /**
   * Unsets the "stoichiometry" attribute of this SpeciesReference.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @note In SBML Level 1, the "stoichiometry" attribute of this 
   *       SpeciesReference object will be just reset to default (1.0) and
   *       isSetStoichiometry() will still return @c true.
   *       In SBML Level 2, the "stoichiometry" attribute of this object will 
   *       be unset (isSetStoichiometry() will return @c false although
   *       getStoichiometry() will return 1.0) if the "stoichiometryMath" 
   *       subelement has been set, otherwise the attribute will be just reset 
   *       to default (1.0) (isSetStoichiometry() will still return @c true).
   *       In SBML Level 3, the "stoichiometry" attribute of this object will
   *       be set to NaN and isSetStoichiometry() will return @c false.
   * 
   *
   */
  int unsetStoichiometry ();


  /**
   * Creates a new, empty StoichiometryMath, adds it to this SpeciesReference
   * and returns the StoichiometryMath.
   *
   * @return the newly created StoichiometryMath object instance
   *
   * @see Reaction::addReactant(const SpeciesReference* sr)
   * @see Reaction::addProduct(const SpeciesReference* sr)
   */
  StoichiometryMath* createStoichiometryMath ();


  /**
   * Sets the value of the "annotation" subelement of this SBML object to a
   * copy of @p annotation.
   *
   * Any existing content of the "annotation" subelement is discarded.
   * Unless you have taken steps to first copy and reconstitute any
   * existing annotations into the @p annotation that is about to be
   * assigned, it is likely that performing such wholesale replacement is
   * unfriendly towards other software applications whose annotations are
   * discarded.  An alternative may be to use appendAnnotation().
   *
   * @param annotation an XML structure that is to be used as the content
   * of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   *
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   */
  virtual int setAnnotation (const XMLNode* annotation);


  /**
   * Sets the value of the "annotation" subelement of this SBML object to a
   * copy of @p annotation.
   *
   * Any existing content of the "annotation" subelement is discarded.
   * Unless you have taken steps to first copy and reconstitute any
   * existing annotations into the @p annotation that is about to be
   * assigned, it is likely that performing such wholesale replacement is
   * unfriendly towards other software applications whose annotations are
   * discarded.  An alternative may be to use appendAnnotation().
   *
   * @param annotation an XML string that is to be used as the content
   * of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   */
  virtual int setAnnotation (const std::string& annotation);


  /**
   * Appends annotation content to any existing content in the "annotation"
   * subelement of this object.
   *
   * The content in @p annotation is copied.  Unlike setAnnotation(), this
   * method allows other annotations to be preserved when an application
   * adds its own data.
   *
   * @param annotation an XML structure that is to be copied and appended
   * to the content of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @see setAnnotation(const std::string& annotation)
   * @see setAnnotation(const XMLNode* annotation)
   */
  virtual int appendAnnotation (const XMLNode* annotation);


  /**
   * Appends annotation content to any existing content in the "annotation"
   * subelement of this object.
   *
   * The content in @p annotation is copied.  Unlike setAnnotation(), this
   * method allows other annotations to be preserved when an application
   * adds its own data.
   *
   * @param annotation an XML string that is to be copied and appended
   * to the content of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @see setAnnotation(const std::string& annotation)
   * @see setAnnotation(const XMLNode* annotation)
   */
  virtual int appendAnnotation (const std::string& annotation);


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
   * Returns the XML element name of this object, which for
   * SpeciesReference, is always @c "speciesReference".
   * 
   * @return the name of this element, i.e., @c "speciesReference".
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond doxygen-libsbml-internal */


  /** @cond doxygen-libsbml-internal */
  /*
   * This functional checks whether a math expression equates to 
   * a rational and produces values for stoichiometry and denominator
   */
  void sortMath();


  /*
   * Sets the "stoichiometry" attribute of SBML Level 2 to 1.0 (default) 
   * if neither the "stoichiometry" attribute nor the "stoichiometrymath" 
   * subelement are present. 
   * (This function is internally invoked in SBase::read().)
   */
  void initL2Stoichiometry();
  /** @endcond doxygen-libsbml-internal */


  /**
   * Predicate returning @c true or @c false depending on whether
   * all the required attributes for this SpeciesReference object
   * have been set.
   *
   * @note The required attributes for a SpeciesReference object are:
   * @li species
   * @li constant (in L3 only)
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
  SpeciesReference ();


  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  bool readOtherXML (XMLInputStream& stream);

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

  /**
   *
   * Synchronizes the annotation of this SBML object.
   *
   * Annotation element (XMLNode* mAnnotation) is synchronized with the
   * current CVTerm objects (List* mCVTerm) and id string (std::string mId)
   * Currently, this method is called in getAnnotation, isSetAnnotation,
   * and writeElements methods.
   */
  virtual void syncAnnotation();


  double    mStoichiometry;
  int       mDenominator;
  StoichiometryMath*  mStoichiometryMath;
  bool      mConstant;
  bool      mIsSetConstant;
  bool      mIsSetStoichiometry;

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

  /** @endcond doxygen-libsbml-internal */
};



class LIBSBML_EXTERN ModifierSpeciesReference : public SimpleSpeciesReference
{
public:

  /**
   * Creates a new ModifierSpeciesReference using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this ModifierSpeciesReference
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ModifierSpeciesReference
   * 
   * @note Once a ModifierSpeciesReference has been added to an SBMLDocument, the @p level,
   * @p version for the document @em override those used
   * to create the ModifierSpeciesReference.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  ModifierSpeciesReference (unsigned int level, unsigned int version);


  /**
   * Creates a new ModifierSpeciesReference using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Once a ModifierSpeciesReference has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the ModifierSpeciesReference.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  ModifierSpeciesReference (SBMLNamespaces* sbmlns);


  /**
   * Destroys this ModifierSpeciesReference.
   */
  virtual ~ModifierSpeciesReference();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this ModifierSpeciesReference
   * instance.
   *
   * @return a (deep) copy of this ModifierSpeciesReference.
   */
  virtual ModifierSpeciesReference* clone () const;


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
   * Returns the XML element name of this object, which for Species, is
   * always @c "modifierSpeciesReference".
   * 
   * @return the name of this element, i.e., @c "modifierSpeciesReference".
   */
  virtual const std::string& getElementName () const;


#ifdef USE_LAYOUT

  /** @cond doxygen-libsbml-internal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond doxygen-libsbml-internal */

#endif // USE_LAYOUT

  /**
   * Predicate returning @c true or @c false depending on whether
   * all the required attributes for this ModifierSpeciesReference object
   * have been set.
   *
   * @note The required attributes for a ModifierSpeciesReference object are:
   * species
   */
  virtual bool hasRequiredAttributes() const ;

protected:

#ifdef USE_LAYOUT
  /** @cond doxygen-libsbml-internal */

  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  bool readOtherXML (XMLInputStream& stream);

  /** @endcond doxygen-libsbml-internal */

#endif // USE_LAYOUT

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  ModifierSpeciesReference ();


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

};



class LIBSBML_EXTERN ListOfSpeciesReferences : public ListOf
{
public:

  /**
   * Creates a new, empty ListOfSpeciesReferences.
   */
  ListOfSpeciesReferences ();
 

  /**
   * Creates and returns a deep copy of this ListOfSpeciesReferences
   * instance.
   *
   * @return a (deep) copy of this ListOfSpeciesReferences.
   */
  virtual ListOfSpeciesReferences* clone () const;


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
   * (i.e., SpeciesReference objects, if the list is non-empty).
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
   * For ListOfSpeciesReferences, the XML element name is @c
   * "listOfSpeciesReferences".
   * 
   * @return the name of this element, i.e., @c "listOfSpeciesReferences".
   */
  virtual const std::string& getElementName () const;


  /**
   * Get a SpeciesReference from the ListOfSpeciesReferences.
   *
   * @param n the index number of the SpeciesReference to get.
   * 
   * @return the nth SpeciesReference in this ListOfSpeciesReferences.
   *
   * @see size()
   */
  virtual SimpleSpeciesReference * get(unsigned int n); 


  /**
   * Get a SpeciesReference from the ListOfSpeciesReferences.
   *
   * @param n the index number of the SpeciesReference to get.
   * 
   * @return the nth SpeciesReference in this ListOfSpeciesReferences.
   *
   * @see size()
   */
  virtual const SimpleSpeciesReference * get(unsigned int n) const; 


  /**
   * Get a SpeciesReference from the ListOfSpeciesReferences
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the SpeciesReference to get.
   * 
   * @return SpeciesReference in this ListOfSpeciesReferences
   * with the given id or NULL if no such
   * SpeciesReference exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual SimpleSpeciesReference* get (const std::string& sid);


  /**
   * Get a SpeciesReference from the ListOfSpeciesReferences
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the SpeciesReference to get.
   * 
   * @return SpeciesReference in this ListOfSpeciesReferences
   * with the given id or NULL if no such
   * SpeciesReference exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const SimpleSpeciesReference* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfSpeciesReferences items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   *
   * @see size()
   */
  virtual SimpleSpeciesReference* remove (unsigned int n);


  /**
   * Removes item in this ListOfSpeciesReferences items with the given identifier.
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
  virtual SimpleSpeciesReference* remove (const std::string& sid);


  /** @cond doxygen-libsbml-internal */

  /**
   * Get the ordinal position of this element in the containing object
   * (which in this case is the Model object).
   *
   * @return the ordinal position of the element with respect to its
   * siblings, or @c -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /** @endcond doxygen-libsbml-internal */


protected:
  /** @cond doxygen-libsbml-internal */

  enum SpeciesType { Unknown, Reactant, Product, Modifier };


  /**
   * Sets type of this ListOfSpeciesReferences.
   */
  void setType (SpeciesType type);


  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


  SpeciesType mType;


  friend class Reaction;

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
SpeciesReference_t *
SpeciesReference_create (unsigned int level, unsigned int version);


LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createModifier (unsigned int level, unsigned int version);


LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createModifierWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
void
SpeciesReference_free (SpeciesReference_t *sr);


LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_clone (const SpeciesReference_t *sr);


LIBSBML_EXTERN
void
SpeciesReference_initDefaults (SpeciesReference_t *sr);


LIBSBML_EXTERN
const XMLNamespaces_t *
SpeciesReference_getNamespaces(SpeciesReference_t *c);


LIBSBML_EXTERN
int
SpeciesReference_isModifier (const SpeciesReference_t *sr);


LIBSBML_EXTERN
const char *
SpeciesReference_getId (const SpeciesReference_t *sr);


LIBSBML_EXTERN
const char *
SpeciesReference_getName (const SpeciesReference_t *sr);


LIBSBML_EXTERN
const char *
SpeciesReference_getSpecies (const SpeciesReference_t *sr);


LIBSBML_EXTERN
double
SpeciesReference_getStoichiometry (const SpeciesReference_t *sr);


LIBSBML_EXTERN
StoichiometryMath_t *
SpeciesReference_getStoichiometryMath (SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_getDenominator (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_getConstant (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_isSetId (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_isSetName (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_isSetSpecies (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_isSetStoichiometryMath (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_isSetStoichiometry (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_isSetConstant (const SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_setId (SpeciesReference_t *sr, const char *sid);


LIBSBML_EXTERN
int
SpeciesReference_setName (SpeciesReference_t *sr, const char *name);


LIBSBML_EXTERN
int
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sid);


LIBSBML_EXTERN
int
SpeciesReference_setStoichiometry (SpeciesReference_t *sr, double value);


LIBSBML_EXTERN
int
SpeciesReference_setStoichiometryMath (  SpeciesReference_t *sr
                                       , const StoichiometryMath_t    *math );


LIBSBML_EXTERN
int
SpeciesReference_setDenominator (SpeciesReference_t *sr, int value);


LIBSBML_EXTERN
int
SpeciesReference_setConstant (SpeciesReference_t *sr, int value);


LIBSBML_EXTERN
int
SpeciesReference_unsetId (SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_unsetName (SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_unsetStoichiometryMath (SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_unsetStoichiometry (SpeciesReference_t *sr);


LIBSBML_EXTERN
int
SpeciesReference_hasRequiredAttributes (SpeciesReference_t *sr);


LIBSBML_EXTERN
StoichiometryMath_t *
SpeciesReference_createStoichiometryMath (SpeciesReference_t *sr);


LIBSBML_EXTERN
SpeciesReference_t *
ListOfSpeciesReferences_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
SpeciesReference_t *
ListOfSpeciesReferences_removeById (ListOf_t *lo, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* SpeciesReference_h */
