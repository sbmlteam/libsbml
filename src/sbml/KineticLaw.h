/**
 * @file    KineticLaw.h
 * @brief   Definition of KineticLaw
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
 * @class KineticLaw
 * @brief LibSBML implementation of %SBML's %KineticLaw construct.
 *
 * An object of class KineticLaw is used to describe the rate at which the
 * process defined by a given Reaction takes place.  KineticLaw has
 * subelements called "math" (for MathML content) and "listOfParameters"
 * (of class ListOfParameters), in addition to the attributes and
 * subelements it inherits from SBase.
 *
 * KineticLaw's "math" subelement for holding a MathML formula defines the
 * rate of the reaction.  The formula may refer to other entities in a
 * model (Compartment, Species, Parameter, FunctionDefinition, Reaction),
 * but the only Species identifiers that can be used in this formula are
 * those declared in the lists of reactants, products and modifiers in the
 * Reaction structure.  Parameter identifiers may be taken from either the
 * KineticLaw's list of local parameters (discussed below) or the
 * parameters defined globally on the Model instance.
 *
 * KineticLaw also provides a way to define @em local parameters whose
 * identifiers can be used in the "math" formula of that KineticLaw
 * instance.  Prior to SBML Level&nbsp;3, these parameter definitions are
 * stored inside a "listOfParameters" subelement containing Parameter
 * objects; in SBML Level&nbsp;3, this is achieved using a specialized
 * object class called LocalParameter and the containing subelement is
 * called "listOfLocalParameters".  In both cases, the parameters so
 * defined are only visible within the KineticLaw; they cannot be accessed
 * outside.  A local parameter within one reaction is not visible from
 * within another, nor is it visible to any other construct outside of the
 * KineticLaw in which it is defined.  In addition, another important
 * feature is that if such a Parameter (or in Level&nbsp;3, LocalParameter)
 * object has the same identifier as another object in the scope of the
 * enclosing Model, the definition inside the KineticLaw takes precedence.
 * In other words, within the KineticLaw's "math" formula, references to
 * local parameter identifiers <strong>shadow any identical global
 * identifiers</strong>.
 *
 * The values of local parameters defined within KineticLaw objects cannot
 * change.  In SBML Level&nbsp;3, this quality is built into the
 * LocalParameter construct.  In Level&nbsp;2, where the same kind of
 * Parameter object class is used as for global parameters, the Parameter
 * objects' "constant" attribute must always have a value of @c true
 * (either explicitly or left to its default value).
 *
 *
 * @note Before SBML Level&nbsp;2 Version&nbsp;2, the SBML specification
 * included two additional attributes on KineticLaw called "substanceUnits"
 * and "timeUnits".  They were removed beginning with SBML Level&nbsp;2
 * Version&nbsp;2 because further research determined they introduced many
 * problems.  The most significant problem was that their use could easily
 * lead to the creation of valid models whose reactions nevertheless could
 * not be integrated into a system of equations without outside knowledge
 * for converting the quantities used.  Examination of real-life models
 * revealed that a common reason for using "substanceUnits" on KineticLaw
 * was to set the units of all reactions to the same set of substance
 * units, something that is better achieved by using UnitDefinition to
 * redefine @c "substance" for the whole Model.
 */


#ifndef KineticLaw_h
#define KineticLaw_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/Parameter.h>
#include <sbml/LocalParameter.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class ASTNode;
class Parameter;
class SBMLVisitor;
class LocalParameter;


class LIBSBML_EXTERN KineticLaw : public SBase
{
public:

  /**
   * Creates a new KineticLaw using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this KineticLaw
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * KineticLaw
   * 
   * @note Upon the addition of a KineticLaw object to an SBMLDocument
   * (e.g., using Model::addKineticLaw()), the SBML Level, SBML Version
   * and XML namespace of the document @em override the values used
   * when creating the KineticLaw object via this constructor.  This is
   * necessary to ensure that an SBML document is a consistent structure.
   * Nevertheless, the ability to supply the values at the time of creation
   * of a KineticLaw is an important aid to producing valid SBML.  Knowledge
   * of the intented SBML Level and Version determine whether it is valid
   * to assign a particular value to an attribute, or whether it is valid
   * to add an object to an existing SBMLDocument.
   */
  KineticLaw (unsigned int level, unsigned int version);


  /**
   * Creates a new KineticLaw using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp;3 Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Upon the addition of a KineticLaw object to an SBMLDocument
   * (e.g., using Model::addKineticLaw()), the SBML XML namespace of the
   * document @em overrides the value used when creating the KineticLaw
   * object via this constructor.  This is necessary to ensure that an SBML
   * document is a consistent structure.  Nevertheless, the ability to
   * supply the values at the time of creation of a KineticLaw is an
   * important aid to producing valid SBML.  Knowledge of the intented SBML
   * Level and Version determine whether it is valid to assign a particular
   * value to an attribute, or whether it is valid to add an object to an
   * existing SBMLDocument.
   */
  KineticLaw (SBMLNamespaces* sbmlns);


  /**
   * Destroys this KineticLaw.
   */
  virtual ~KineticLaw ();


  /**
   * Copy constructor; creates a copy of this KineticLaw.
   */
  KineticLaw (const KineticLaw& orig);


  /**
   * Assignment operator for KineticLaw.
   */
  KineticLaw& operator=(const KineticLaw& rhs);


  /**
   * Accepts the given SBMLVisitor for this instance of KineticLaw.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this KineticLaw object.
   *
   * @return a (deep) copy of this KineticLaw.
   */
  virtual KineticLaw* clone () const;


  /**
   * Returns the mathematical formula for this KineticLaw object and return
   * it as as a text string.
   *
   * This is fundamentally equivalent to getMath().  This variant is
   * provided principally for compatibility compatibility with SBML Level
   * 1.
   * 
   * @return a string representing the formula of this KineticLaw.
   *
   * @see getMath()
   *
   * @note SBML Level&nbsp;1 uses a text-string format for mathematical
   * formulas.  SBML Level&nbsp;2 uses MathML, an XML format for
   * representing mathematical expressions.  LibSBML provides an Abstract
   * Syntax Tree API for working with mathematical expressions; this API is
   * more powerful than working with formulas directly in text form, and
   * ASTs can be translated into either MathML or the text-string syntax.
   * The libSBML methods that accept text-string formulas directly (such as
   * this constructor) are provided for SBML Level&nbsp;1 compatibility,
   * but developers are encouraged to use the AST mechanisms.
   */
  const std::string& getFormula () const;


  /**
   * Returns the mathematical formula for this KineticLaw object and return
   * it as as an AST.
   *
   * This is fundamentally equivalent to getFormula().  The latter is
   * provided principally for compatibility compatibility with SBML Level
   * 1, which represented mathematical formulas in text-string form.
   * 
   * @return the ASTNode representation of the mathematical formula.
   *
   * @see getFormula()
   */
  const ASTNode* getMath () const;


  /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Returns the value of the "timeUnits" attribute of this KineticLaw
   * object.
   *
   * @return the "timeUnits" attribute value
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   */
  const std::string& getTimeUnits () const;


  /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Returns the value of the
   * "substanceUnits" attribute of this KineticLaw object.
   *
   * @return the "substanceUnits" attribute value
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   */
  const std::string& getSubstanceUnits () const;


  /**
   * Predicate returning @c true if this
   * KineticLaw's "formula" attribute has been set
   *
   * @htmlinclude comment-set-methods.html
   *
   * This is functionally identical to the method isSetMath().  It is
   * provided in order to mirror the parallel between getFormula() and
   * getMath().
   *
   * @return @c true if the formula (meaning the @c math subelement) of
   * this KineticLaw has been set, @c false otherwise.
   *
   * @note SBML Level&nbsp;1 uses a text-string format for mathematical
   * formulas.  SBML Level&nbsp;2 uses MathML, an XML format for
   * representing mathematical expressions.  LibSBML provides an Abstract
   * Syntax Tree API for working with mathematical expressions; this API is
   * more powerful than working with formulas directly in text form, and
   * ASTs can be translated into either MathML or the text-string syntax.
   * The libSBML methods that accept text-string formulas directly (such as
   * this constructor) are provided for SBML Level&nbsp;1 compatibility,
   * but developers are encouraged to use the AST mechanisms.
   */  
  bool isSetFormula () const;


  /**
   * Predicate returning @c true if this
   * Kinetic's "math" subelement has been set
   *
   * @htmlinclude comment-set-methods.html
   *
   * This is identical to the method isSetFormula().  It is provided
   * in order to mirror the parallel between getFormula() and getMath().
   * 
   * @return @c true if the formula (meaning the @c math subelement) of
   * this KineticLaw has been set, @c false otherwise.
   */
  bool isSetMath () const;


 /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Predicate returning @c true if
   * this SpeciesReference's "timeUnits" attribute has been set
   *
   * @htmlinclude comment-set-methods.html
   *
   * @return @c true if the "timeUnits" attribute of this KineticLaw object
   * has been set, @c false otherwise.
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   */
  bool isSetTimeUnits () const;


 /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Predicate returning @c true if
   * this SpeciesReference's "substanceUnits" attribute has been set
   *
   * @htmlinclude comment-set-methods.html
   *
   * @return @c true if the "substanceUnits" attribute of this KineticLaw
   * object has been set, @c false otherwise.
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   */
  bool isSetSubstanceUnits () const;


  /**
   * Sets the mathematical expression of this KineticLaw instance to the
   * given @p formula.
   *
   * @htmlinclude comment-set-methods.html
   *
   * The given @p formula string is copied.  Internally, libSBML stores the
   * mathematical expression as an ASTNode.
   *
   * @param formula the mathematical expression to use, represented in
   * text-string form.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   *
   * @note SBML Level&nbsp;1 uses a text-string format for mathematical
   * formulas.  SBML Level&nbsp;2 uses MathML, an XML format for representing
   * mathematical expressions.  LibSBML provides an Abstract Syntax Tree
   * API for working with mathematical expressions; this API is more
   * powerful than working with formulas directly in text form, and ASTs
   * can be translated into either MathML or the text-string syntax.  The
   * libSBML methods that accept text-string formulas directly (such as
   * this constructor) are provided for SBML Level&nbsp;1 compatibility, but
   * developers are encouraged to use the AST mechanisms.
   */
  int setFormula (const std::string& formula);


  /**
   * Sets the mathematical expression of this KineticLaw instance to a copy
   * of the given ASTNode.
   *
   * @htmlinclude comment-set-methods.html
   *
   * This is fundamentally identical to setFormula().  The latter is
   * provided principally for compatibility compatibility with SBML Level
   * 1, which represented mathematical formulas in text-string form.
   *
   * @param math an ASTNode representing a formula tree.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
  */
  int setMath (const ASTNode* math);


  /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Sets the "timeUnits" attribute
   * of this KineticLaw object to a copy of the identifier in @p sid.
   *
   * @htmlinclude comment-set-methods.html
   *
   * @param sid the identifier of the units to use.
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int setTimeUnits (const std::string& sid);


  /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Sets the "substanceUnits"
   * attribute of this KineticLaw object to a copy of the identifier given
   * in @p sid.
   *
   * @htmlinclude comment-set-methods.html
   *
   * @param sid the identifier of the units to use.
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int setSubstanceUnits (const std::string& sid);


  /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Unsets the "timeUnits"
   * attribugte of this KineticLaw object.
   *
   * @htmlinclude comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   */
  int unsetTimeUnits ();


  /**
   * (SBML Level&nbsp;2 Version&nbsp;1 only) Unsets the "substanceUnits"
   * attribute of this KineticLaw object.
   *
   * @htmlinclude comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @note In SBML Level&nbsp;2 Version&nbsp;2, the "timeUnits" and
   * "substanceUnits" attributes were removed.  For compatibility with new
   * versions of SBML, users are cautioned to avoid these attributes.
   */
  int unsetSubstanceUnits ();


  /**
   * Adds a copy of the given Parameter object to the list of local
   * parameters in this KineticLaw.
   *
   * @param p the Parameter to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_LEVEL_MISMATCH LIBSBML_LEVEL_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_VERSION_MISMATCH LIBSBML_VERSION_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_DUPLICATE_OBJECT_ID LIBSBML_DUPLICATE_OBJECT_ID @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this KineticLaw.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the KineticLaw</em>.  In addition, the caller should make
   * sure to free the original object if it is no longer being used, or
   * else a memory leak will result.  Please see
   * KineticLaw::createParameter() for ab method that does not lead to
   * these issues.
   *
   * @see createParameter()
   */
  int addParameter (const Parameter* p);


  /**
   * Adds a copy of the given LocalParameter object to the list of local
   * parameters in this KineticLaw.
   *
   * @param p the LocalParameter to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_LEVEL_MISMATCH LIBSBML_LEVEL_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_VERSION_MISMATCH LIBSBML_VERSION_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_DUPLICATE_OBJECT_ID LIBSBML_DUPLICATE_OBJECT_ID @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this KineticLaw.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the KineticLaw</em>.  In addition, the caller should make
   * sure to free the original object if it is no longer being used, or
   * else a memory leak will result.  Please see
   * KineticLaw::createParameter() for ab method that does not lead to
   * these issues.
   *
   * @see createLocalParameter()
   */
  int addLocalParameter (const LocalParameter* p);


  /**
   * Creates a new Parameter object, adds it to this KineticLaw's list of
   * local parameters, and returns the Parameter object created.
   *
   * @return a new Parameter object instance
   *
   * @see addParameter(const Parameter* p)
   */
  Parameter* createParameter ();


  /**
   * Creates a new LocalParameter object, adds it to this KineticLaw's list of
   * local parameters, and returns the LocalParameter object created.
   *
   * @return a new LocalParameter object instance
   *
   * @see addLocalParameter(const LocalParameter* p)
   */
  LocalParameter* createLocalParameter ();


  /**
   * Returns the list of local parameters in this KineticLaw object.
   * 
   * @return the list of Parameters for this KineticLaw.
   */
  const ListOfParameters* getListOfParameters () const;


  /**
   * Returns the list of local parameters in this KineticLaw object.
   * 
   * @return the list of Parameters for this KineticLaw.
   */
  ListOfParameters* getListOfParameters ();


  /**
   * Returns the list of local parameters in this KineticLaw object.
   * 
   * @return the list of LocalParameters for this KineticLaw.
   */
  const ListOfLocalParameters* getListOfLocalParameters () const;


  /**
   * Returns the list of local parameters in this KineticLaw object.
   * 
   * @return the list of LocalParameters for this KineticLaw.
   */
  ListOfLocalParameters* getListOfLocalParameters ();


  /**
   * Returns the nth Parameter object in the list of local parameters in
   * this KineticLaw instance.
   *
   * @param n the index of the Parameter object sought
   * 
   * @return the nth Parameter of this KineticLaw.
   */
  const Parameter* getParameter (unsigned int n) const;


  /**
   * Returns the nth Parameter object in the list of local parameters in
   * this KineticLaw instance.
   *
   * @param n the index of the Parameter object sought
   * 
   * @return the nth Parameter of this KineticLaw.
   */
  Parameter* getParameter (unsigned int n);


  /**
   * Returns the nth LocalParameter object in the list of local parameters in
   * this KineticLaw instance.
   *
   * @param n the index of the LocalParameter object sought
   * 
   * @return the nth LocalParameter of this KineticLaw.
   */
  const LocalParameter* getLocalParameter (unsigned int n) const;


  /**
   * Returns the nth LocalParameter object in the list of local parameters in
   * this KineticLaw instance.
   *
   * @param n the index of the LocalParameter object sought
   * 
   * @return the nth LocalParameter of this KineticLaw.
   */
  LocalParameter* getLocalParameter (unsigned int n);


  /**
   * Returns a local parameter based on its identifier.
   *
   * @param sid the identifier of the Parameter being sought.
   * 
   * @return the Parameter object in this KineticLaw instace having the
   * given "id", or @c NULL if no such Parameter exists.
   */
  const Parameter* getParameter (const std::string& sid) const;


  /**
   * Returns a local parameter based on its identifier.
   *
   * @param sid the identifier of the Parameter being sought.
   * 
   * @return the Parameter object in this KineticLaw instace having the
   * given "id", or @c NULL if no such Parameter exists.
   */
  Parameter* getParameter (const std::string& sid);


  /**
   * Returns a local parameter based on its identifier.
   *
   * @param sid the identifier of the LocalParameter being sought.
   * 
   * @return the LocalParameter object in this KineticLaw instace having the
   * given "id", or @c NULL if no such LocalParameter exists.
   */
  const LocalParameter* getLocalParameter (const std::string& sid) const;


  /**
   * Returns a local parameter based on its identifier.
   *
   * @param sid the identifier of the LocalParameter being sought.
   * 
   * @return the LocalParameter object in this KineticLaw instace having the
   * given "id", or @c NULL if no such LocalParameter exists.
   */
  LocalParameter* getLocalParameter (const std::string& sid);


  /**
   * Returns the number of local parameters in this KineticLaw instance.
   * 
   * @return the number of Parameters in this KineticLaw.
   */
  unsigned int getNumParameters () const;


  /**
   * Returns the number of local parameters in this KineticLaw instance.
   * 
   * @return the number of LocalParameters in this KineticLaw.
   */
  unsigned int getNumLocalParameters () const;


  /**
   * Calculates and returns a UnitDefinition that expresses the units
   * of measurement assumed for the "math" expression of this
   * KineticLaw.
   *
   * The units are calculated based on the mathematical expression in the
   * KineticLaw and the model quantities referenced by
   * <code>&lt;ci&gt;</code> elements used within that expression.  The
   * getDerivedUnitDefinition() method returns the calculated units.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return @c NULL.
   *
   * @warning Note that it is possible the "math" expression in the
   * KineticLaw contains pure numbers or parameters with undeclared
   * units.  In those cases, it is not possible to calculate the units of
   * the overall expression without making assumptions.  LibSBML does not
   * make assumptions about the units, and getDerivedUnitDefinition() only
   * returns the units as far as it is able to determine them.  For
   * example, in an expression <em>X + Y</em>, if <em>X</em> has
   * unambiguously-defined units and <em>Y</em> does not, it will return
   * the units of <em>X</em>.  <strong>It is important that callers also
   * invoke the method</strong> containsUndeclaredUnits() <strong>to
   * determine whether this situation holds</strong>.  Callers may wish to
   * take suitable actions in those scenarios.
   *
   * @return a UnitDefinition that expresses the units of the math 
   * expression of this KineticLaw, or @c NULL if one cannot be constructed.
   *
   * @see containsUndeclaredUnits()
   */
  UnitDefinition * getDerivedUnitDefinition();


  /**
   * Calculates and returns a UnitDefinition that expresses the units
   * of measurement assumed for the "math" expression of this
   * KineticLaw.
   *
   * The units are calculated based on the mathematical expression in the
   * KineticLaw and the model quantities referenced by
   * <code>&lt;ci&gt;</code> elements used within that expression.  The
   * getDerivedUnitDefinition() method returns the calculated units.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return @c NULL.
   *
   * @warning Note that it is possible the "math" expression in the
   * KineticLaw contains pure numbers or parameters with undeclared
   * units.  In those cases, it is not possible to calculate the units of
   * the overall expression without making assumptions.  LibSBML does not
   * make assumptions about the units, and getDerivedUnitDefinition() only
   * returns the units as far as it is able to determine them.  For
   * example, in an expression <em>X + Y</em>, if <em>X</em> has
   * unambiguously-defined units and <em>Y</em> does not, it will return
   * the units of <em>X</em>.  <strong>It is important that callers also
   * invoke the method</strong> containsUndeclaredUnits() <strong>to
   * determine whether this situation holds</strong>.  Callers may wish to
   * take suitable actions in those scenarios.
   *
   * @return a UnitDefinition that expresses the units of the math 
   * expression of this KineticLaw, or @c NULL if one cannot be constructed.
   *
   * @see containsUndeclaredUnits()
   */
  const UnitDefinition * getDerivedUnitDefinition() const;


  /**
   * Predicate returning @c true if 
   * the math expression of this KineticLaw contains
   * parameters/numbers with undeclared units.
   * 
   * @return @c true if the math expression of this KineticLaw
   * includes parameters/numbers 
   * with undeclared units, @c false otherwise.
   *
   * @note A return value of @c true indicates that the UnitDefinition
   * returned by getDerivedUnitDefinition() may not accurately represent
   * the units of the expression.
   *
   * @see getDerivedUnitDefinition()
   */
  bool containsUndeclaredUnits();


  /**
   * Predicate returning @c true if 
   * the math expression of this KineticLaw contains
   * parameters/numbers with undeclared units.
   * 
   * @return @c true if the math expression of this KineticLaw
   * includes parameters/numbers 
   * with undeclared units, @c false otherwise.
   *
   * @note A return value of @c true indicates that the UnitDefinition
   * returned by getDerivedUnitDefinition() may not accurately represent
   * the units of the expression.
   *
   * @see getDerivedUnitDefinition()
   */
  bool containsUndeclaredUnits() const;


  /**
   * Removes the nth Parameter object in the list of local parameters 
   * in this KineticLaw instance and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the Parameter object to remove
   * 
   * @return the Parameter object removed.  As mentioned above, 
   * the caller owns the returned item. @c NULL is returned if the given index 
   * is out of range.
   */
  Parameter* removeParameter (unsigned int n);


  /**
   * Removes the nth LocalParameter object in the list of local parameters 
   * in this KineticLaw instance and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the LocalParameter object to remove
   * 
   * @return the LocalParameter object removed.  As mentioned above, 
   * the caller owns the returned item. @c NULL is returned if the given index 
   * is out of range.
   */
  LocalParameter* removeLocalParameter (unsigned int n);


  /**
   * Removes a Parameter object with the given identifier in the list of
   * local parameters in this KineticLaw instance and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param sid the identifier of the Parameter to remove
   * 
   * @return the Parameter object removed.  As mentioned above, the 
   * caller owns the returned object. @c NULL is returned if no Parameter
   * object with the identifier exists in this KineticLaw instance.
   */
  Parameter* removeParameter (const std::string& sid);


  /**
   * Removes a LocalParameter object with the given identifier in the list of
   * local parameters in this KineticLaw instance and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param sid the identifier of the LocalParameter to remove
   * 
   * @return the LocalParameter object removed.  As mentioned above, the 
   * caller owns the returned object. @c NULL is returned if no LocalParameter
   * object with the identifier exists in this KineticLaw instance.
   */
  LocalParameter* removeLocalParameter (const std::string& sid);


  /** @cond doxygen-libsbml-internal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument to use.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Sets the parent SBML object of this SBML object.
   *
   * @param sb the SBML object to use
   */
  virtual void setParentSBMLObject (SBase* sb);
  /** @endcond */


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
   * @return the SBML type code for this object, or @link
   * SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Returns the XML element name of this object, which for Species, is
   * always @c "kineticLaw".
   * 
   * @return the name of this element, i.e., @c "kineticLaw".
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */
  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /**
   * Predicate returning @c true if
   * all the required attributes for this KineticLaw object
   * have been set.
   *
   * @note The required attributes for a KineticLaw object are:
   * formula (L1 only)
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const ;


  /**
   * Predicate returning @c true if
   * all the required elements for this KineticLaw object
   * have been set.
   *
   * @note The required elements for a KineticLaw object are:
   * math
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const ;


  /** @cond doxygen-libsbml-internal */
  /*
   * Function to set/get an identifier for unit checking.
   * */
  std::string getInternalId() const { return mInternalId; };
  void setInternalId(std::string id) { mInternalId = id; };
  /** @endcond */


protected:
  /** @cond doxygen-libsbml-internal */

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  KineticLaw ();


  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


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

  void readL1Attributes (const XMLAttributes& attributes);

  void readL2Attributes (const XMLAttributes& attributes);
  
  void readL3Attributes (const XMLAttributes& attributes);


  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  mutable std::string  mFormula;
  mutable ASTNode*     mMath;

  ListOfParameters  mParameters;
  ListOfLocalParameters  mLocalParameters;
  std::string       mTimeUnits;
  std::string       mSubstanceUnits;

  /* internal id used by unit checking */
  std::string mInternalId;

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

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (unsigned int level, unsigned int version);


LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl);


LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_clone (const KineticLaw_t *kl);


LIBSBML_EXTERN
const XMLNamespaces_t *
KineticLaw_getNamespaces(KineticLaw_t *kl);


LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl);


LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl);


LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl);


LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl);


LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl);


LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl);


LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl);


LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl);


LIBSBML_EXTERN
int
KineticLaw_setFormula (KineticLaw_t *kl, const char *formula);


LIBSBML_EXTERN
int
KineticLaw_setMath (KineticLaw_t *kl, const ASTNode_t *math);


LIBSBML_EXTERN
int
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sid);


LIBSBML_EXTERN
int
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sid);


LIBSBML_EXTERN
int
KineticLaw_unsetTimeUnits (KineticLaw_t *kl);


LIBSBML_EXTERN
int
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl);


LIBSBML_EXTERN
int
KineticLaw_addParameter (KineticLaw_t *kl, const Parameter_t *p);


LIBSBML_EXTERN
int
KineticLaw_addLocalParameter (KineticLaw_t *kl, const LocalParameter_t *p);


LIBSBML_EXTERN
Parameter_t *
KineticLaw_createParameter (KineticLaw_t *kl);


LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_createLocalParameter (KineticLaw_t *kl);


LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (KineticLaw_t *kl);


LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfLocalParameters (KineticLaw_t *kl);


LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (KineticLaw_t *kl, unsigned int n);


LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_getLocalParameter (KineticLaw_t *kl, unsigned int n);


LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameterById (KineticLaw_t *kl, const char *sid);


LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_getLocalParameterById (KineticLaw_t *kl, const char *sid);


LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl);


LIBSBML_EXTERN
unsigned int
KineticLaw_getNumLocalParameters (const KineticLaw_t *kl);


LIBSBML_EXTERN
UnitDefinition_t * 
KineticLaw_getDerivedUnitDefinition(KineticLaw_t *kl);


LIBSBML_EXTERN
int 
KineticLaw_containsUndeclaredUnits(KineticLaw_t *kl);


LIBSBML_EXTERN
Parameter_t *
KineticLaw_removeParameter (KineticLaw_t *kl, unsigned int n);


LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_removeLocalParameter (KineticLaw_t *kl, unsigned int n);


LIBSBML_EXTERN
Parameter_t *
KineticLaw_removeParameterById (KineticLaw_t *kl, const char *sid);


LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_removeLocalParameterById (KineticLaw_t *kl, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* KineticLaw_h */
