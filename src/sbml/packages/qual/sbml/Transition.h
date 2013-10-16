/**
 * @file:   Transition.h
 * @brief:  Implementation of the Transition class
 * @author: Generated by autocreate code
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class Transition
 * @ingroup qual
 * @brief @htmlinclude pkg-marker-qual.html
 * The Transition class for the Qualitative Models package.
 *
 * A Transition element contains at most one ListOfInputs and one ListOfOutputs and exactly one ListOfFunctionTerms.
 *
 * A Transition defines the changes in level associated with the QualitativeSpecies that occur when a Transition is enabled.
 * 
 * In logical models, a Transition is used to specify the logical rule associated with a QualitativeSpecies (that appears as an Output of this Transition). For example, the rule "if A > 1 : B = 2" would be encapsulated as a Transition with QualitativeSpecies "A" as an Input and "B" as an Output; the "if A > 1" rule being encode by the math element of a FunctionTerm with the resultLevel attribute having a value "2". 
 *
 * In Petri net models, a Transition is interpreted, using the common Petri net semantics, as events that might occur within the system causing tokens to be moved.
 *
 * 
 */


#ifndef Transition_H__
#define Transition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/qual/common/qualfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/qual/extension/QualExtension.h>

#include <sbml/packages/qual/sbml/Input.h>
#include <sbml/packages/qual/sbml/Output.h>
#include <sbml/packages/qual/sbml/FunctionTerm.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Transition : public SBase
{

protected:

  std::string   mId;
  std::string   mName;
  ListOfInputs   mInputs;
  ListOfOutputs   mOutputs;
  ListOfFunctionTerms   mFunctionTerms;


public:

  /**
   * Creates a new Transition with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Transition
   *
   * @param version an unsigned int, the SBML Version to assign to this Transition
   *
   * @param pkgVersion an unsigned int, the SBML Qual Version to assign to this Transition
   */
  Transition(unsigned int level      = QualExtension::getDefaultLevel(),
             unsigned int version    = QualExtension::getDefaultVersion(),
             unsigned int pkgVersion = QualExtension::getDefaultPackageVersion());


  /**
   * Creates a new Transition with the given QualPkgNamespaces object.
   *
   * @param qualns the QualPkgNamespaces object
   */
  Transition(QualPkgNamespaces* qualns);


  /**
   * Copy constructor for Transition.
   *
   * @param orig; the Transition instance to copy.
   */
  Transition(const Transition& orig);


  /**
   * Assignment operator for Transition.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  Transition& operator=(const Transition& rhs);


  /**
   * Creates and returns a deep copy of this Transition object.
   *
   * @return a (deep) copy of this Transition object.
   */
  virtual Transition* clone () const;


  /**
   * Destructor for Transition.
   */
  virtual ~Transition();


  /**
   * Returns the first child element found that has the given @p id 
   * in the model-wide SId namespace, or @c NULL if no such object is found.
   *
   * @param id string representing the id of objects to find
   *
   * @return a pointer to the SBase element with the given @p id.
   */
  virtual SBase* getElementBySId(const std::string& id);
  
  
  /**
   * Returns the first child element it can find with the given @p metaid, 
   * or itself if it has the given @p metaid, or @c NULL if no such object 
   * is found.
   *
   * @param metaid string representing the metaid of objects to find
   *
   * @return a pointer to the SBase element with the given @p metaid.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);
  

  /**
   * Returns the value of the "id" attribute of this Transition.
   *
   * @return the value of the "id" attribute of this Transition as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this Transition.
   *
   * @return the value of the "name" attribute of this Transition as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Transition's "id" attribute has been set.
   *
   * @return @c true if this Transition's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Transition's "name" attribute has been set.
   *
   * @return @c true if this Transition's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this Transition.
   *
   * @param id; const std::string& value of the "id" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "name" attribute of this Transition.
   *
   * @param name; const std::string& value of the "name" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "id" attribute of this Transition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this Transition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetName();


  /**
   * Returns the  "ListOfInputs" in this Transition object.
   *
   * @return the "ListOfInputs" attribute of this Transition.
   */
  const ListOfInputs* getListOfInputs() const;


  /**
   * Returns the  "ListOfInputs" in this Transition object.
   *
   * @return the "ListOfInputs" attribute of this Transition.
   */
  ListOfInputs* getListOfInputs();


  /**
   * Get a Input from the ListOfInputs.
   *
   * @param n the index number of the Input to get.
   *
   * @return the nth Input in the ListOfInputs within this Transition.
   *
   * @see getNumInputs()
   */
  Input* getInput(unsigned int n);


  /**
   * Get a Input from the ListOfInputs.
   *
   * @param n the index number of the Input to get.
   *
   * @return the nth Input in the ListOfInputs within this Transition.
   *
   * @see getNumInputs()
   */
  const Input* getInput(unsigned int n) const;


  /**
   * Get a Input from the ListOfInputs
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Input to get.
   *
   * @return the Input in the ListOfInputs
   * with the given id or NULL if no such
   * Input exists.
   *
   * @see getInput(unsigned int n)
   *
   * @see getNumInputs()
   */
  Input* getInput(const std::string& sid);


  /**
   * Get a Input from the ListOfInputs
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Input to get.
   *
   * @return the Input in the ListOfInputs
   * with the given id or NULL if no such
   * Input exists.
   *
   * @see getInput(unsigned int n)
   *
   * @see getNumInputs()
   */
  const Input* getInput(const std::string& sid) const;


  /**
   * Get a Input from the ListOfInputs
   * based on its qualitativeSpecies attribute.
   *
   * @param sid a string representing the qualitativeSpecies
   * of the Input to get.
   *
   * @return the first Input in the ListOfInputs
   * with the given qualitativeSpecies or NULL if no such
   * Input exists.
   *
   * @see getInput(unsigned int n)
   *
   * @see getNumInputs()
   */
  Input* getInputBySpecies(const std::string& sid);


  /**
   * Get a Input from the ListOfInputs
   * based on its qualitativeSpecies attribute.
   *
   * @param sid a string representing the qualitativeSpecies
   * of the Input to get.
   *
   * @return the first Input in the ListOfInputs
   * with the given qualitativeSpecies or NULL if no such
   * Input exists.
   *
   * @see getInput(unsigned int n)
   *
   * @see getNumInputs()
   */
  const Input* getInputBySpecies(const std::string& sid) const;


  /**
   * Adds a copy the given "Input" to this Transition.
   *
   * @param i; the Input object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addInput(const Input* i);


  /**
   * Get the number of Input objects in this Transition.
   *
   * @return the number of Input objects in this Transition
   */
  unsigned int getNumInputs() const;


  /**
   * Creates a new Input object, adds it to this Transitions
   * ListOfInputs and returns the Input object created. 
   *
   * @return a new Input object instance
   *
   * @see addInput(const Input* i)
   */
  Input* createInput();


  /**
   * Removes the nth Input from the ListOfInputs within this Transition.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the Input to remove.
   *
   * @see getNumInputs()
   */
  Input* removeInput(unsigned int n);


  /**
   * Removes the Input with the given identifier from the ListOfInputs within this Transition
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the Input to remove.
   *
   * @return the Input removed. As mentioned above, the caller owns the
   * returned item.
   */
  Input* removeInput(const std::string& sid);


  /**
   * Returns the  "ListOfOutputs" in this Transition object.
   *
   * @return the "ListOfOutputs" attribute of this Transition.
   */
  const ListOfOutputs* getListOfOutputs() const;


  /**
   * Returns the  "ListOfOutputs" in this Transition object.
   *
   * @return the "ListOfOutputs" attribute of this Transition.
   */
  ListOfOutputs* getListOfOutputs();


  /**
   * Get a Output from the ListOfOutputs.
   *
   * @param n the index number of the Output to get.
   *
   * @return the nth Output in the ListOfOutputs within this Transition.
   *
   * @see getNumOutputs()
   */
  Output* getOutput(unsigned int n);


  /**
   * Get a Output from the ListOfOutputs.
   *
   * @param n the index number of the Output to get.
   *
   * @return the nth Output in the ListOfOutputs within this Transition.
   *
   * @see getNumOutputs()
   */
  const Output* getOutput(unsigned int n) const;


  /**
   * Get a Output from the ListOfOutputs
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Output to get.
   *
   * @return the Output in the ListOfOutputs
   * with the given id or NULL if no such
   * Output exists.
   *
   * @see getOutput(unsigned int n)
   *
   * @see getNumOutputs()
   */
  Output* getOutput(const std::string& sid);


  /**
   * Get a Output from the ListOfOutputs
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Output to get.
   *
   * @return the Output in the ListOfOutputs
   * with the given id or NULL if no such
   * Output exists.
   *
   * @see getOutput(unsigned int n)
   *
   * @see getNumOutputs()
   */
  const Output* getOutput(const std::string& sid) const;


  /**
   * Get a Output from the ListOfOutputs
   * based on its qualitativeSpecies attribute.
   *
   * @param sid a string representing the qualitativeSpecies
   * of the Output to get.
   *
   * @return the first Output in the ListOfOutputs
   * with the given qualitativeSpecies or NULL if no such
   * Output exists.
   *
   * @see getOutput(unsigned int n)
   *
   * @see getNumOutputs()
   */
  Output* getOutputBySpecies(const std::string& sid);


  /**
   * Get a Output from the ListOfOutputs
   * based on its qualitativeSpecies attribute.
   *
   * @param sid a string representing the qualitativeSpecies
   * of the Output to get.
   *
   * @return the first Output in the ListOfOutputs
   * with the given qualitativeSpecies or NULL if no such
   * Output exists.
   *
   * @see getOutput(unsigned int n)
   *
   * @see getNumOutputs()
   */
  const Output* getOutputBySpecies(const std::string& sid) const;


  /**
   * Adds a copy the given "Output" to this Transition.
   *
   * @param o; the Output object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addOutput(const Output* o);


  /**
   * Get the number of Output objects in this Transition.
   *
   * @return the number of Output objects in this Transition
   */
  unsigned int getNumOutputs() const;


  /**
   * Creates a new Output object, adds it to this Transitions
   * ListOfOutputs and returns the Output object created. 
   *
   * @return a new Output object instance
   *
   * @see addOutput(const Output* o)
   */
  Output* createOutput();


  /**
   * Removes the nth Output from the ListOfOutputs within this Transition.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the Output to remove.
   *
   * @see getNumOutputs()
   */
  Output* removeOutput(unsigned int n);


  /**
   * Removes the Output with the given identifier from the ListOfOutputs within this Transition
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the Output to remove.
   *
   * @return the Output removed. As mentioned above, the caller owns the
   * returned item.
   */
  Output* removeOutput(const std::string& sid);


  /**
   * Returns the  "ListOfFunctionTerms" in this Transition object.
   *
   * @return the "ListOfFunctionTerms" attribute of this Transition.
   */
  const ListOfFunctionTerms* getListOfFunctionTerms() const;


  /**
   * Returns the  "ListOfFunctionTerms" in this Transition object.
   *
   * @return the "ListOfFunctionTerms" attribute of this Transition.
   */
  ListOfFunctionTerms* getListOfFunctionTerms();


  /**
   * Get a FunctionTerm from the ListOfFunctionTerms.
   *
   * @param n the index number of the FunctionTerm to get.
   *
   * @return the nth FunctionTerm in the ListOfFunctionTerms within this Transition.
   *
   * @see getNumFunctionTerms()
   */
  FunctionTerm* getFunctionTerm(unsigned int n);


  /**
   * Get a FunctionTerm from the ListOfFunctionTerms.
   *
   * @param n the index number of the FunctionTerm to get.
   *
   * @return the nth FunctionTerm in the ListOfFunctionTerms within this Transition.
   *
   * @see getNumFunctionTerms()
   */
  const FunctionTerm* getFunctionTerm(unsigned int n) const;


  /**
   * Get a FunctionTerm from the ListOfFunctionTerms
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FunctionTerm to get.
   *
   * @return the FunctionTerm in the ListOfFunctionTerms
   * with the given id or NULL if no such
   * FunctionTerm exists.
   *
   * @see getFunctionTerm(unsigned int n)
   *
   * @see getNumFunctionTerms()
   */
  FunctionTerm* getFunctionTerm(const std::string& sid);


  /**
   * Get a FunctionTerm from the ListOfFunctionTerms
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FunctionTerm to get.
   *
   * @return the FunctionTerm in the ListOfFunctionTerms
   * with the given id or NULL if no such
   * FunctionTerm exists.
   *
   * @see getFunctionTerm(unsigned int n)
   *
   * @see getNumFunctionTerms()
   */
  const FunctionTerm* getFunctionTerm(const std::string& sid) const;


  /**
   * Adds a copy the given "FunctionTerm" to this Transition.
   *
   * @param ft; the FunctionTerm object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addFunctionTerm(const FunctionTerm* ft);


  /**
   * Get the number of FunctionTerm objects in this Transition.
   *
   * @return the number of FunctionTerm objects in this Transition
   */
  unsigned int getNumFunctionTerms() const;


  /**
   * Creates a new FunctionTerm object, adds it to this Transitions
   * ListOfFunctionTerms and returns the FunctionTerm object created. 
   *
   * @return a new FunctionTerm object instance
   *
   * @see addFunctionTerm(const FunctionTerm* ft)
   */
  FunctionTerm* createFunctionTerm();


  /**
   * Removes the nth FunctionTerm from the ListOfFunctionTerms within this Transition.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the FunctionTerm to remove.
   *
   * @see getNumFunctionTerms()
   */
  FunctionTerm* removeFunctionTerm(unsigned int n);


  /**
   * Removes the FunctionTerm with the given identifier from the ListOfFunctionTerms within this Transition
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the FunctionTerm to remove.
   *
   * @return the FunctionTerm removed. As mentioned above, the caller owns the
   * returned item.
   */
  FunctionTerm* removeFunctionTerm(const std::string& sid);


  /**
   * Creates a new DefaultTerm object, adds it to this Transitions
   * ListOfFunctionTerms and returns the DefaultTerm object created. 
   *
   * @return a new DefaultTerm object instance
   *
   * @see setDefaultTerm(const DefaultTerm* ft)
   */
   DefaultTerm* createDefaultTerm();


  /**
   * Sets the given "DefaultTerm" to this Transition.
   *
   * @param dt; the DefaultTerm object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setDefaultTerm(const DefaultTerm* dt);


  /**
   * Predicate returning @c true if the defaultTerm
   * for this Transition object has been set.
   *
   * @return a boolean value indicating whether the defaultTerm
   * child for this object has been defined.
   */
  bool isSetDefaultTerm() const;
  
  /**
   * Get the DefaultTerm from the ListOfFunctionTerms.
   *
   * @return the DefaultTerm in the ListOfFunctionTerms within this Transition, or NULL if no such value is set.
   */
  DefaultTerm* getDefaultTerm();

  /**
   * Get the DefaultTerm from the ListOfFunctionTerms.
   *
   * @return the DefaultTerm in the ListOfFunctionTerms within this Transition, or NULL if no such value is set.
   */
  const DefaultTerm* getDefaultTerm() const;
  
  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for Transition, is
   * always @c "transition".
   *
   * @return the name of this element, i.e. @c "transition".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code of this object instance.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @link SBMLQualTypeCode_t#SBML_QUAL_TRANSITION SBML_QUAL_TRANSITION@endlink
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this Transition object have been set.
   *
   * @note The required attributes for a Transition object are:
   * @li "output"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this Transition object have been set.
   *
   * @note The required elements for a Transition object are:
   * @li "output"
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};

/**
 * @class ListOfTransitions
 * @ingroup qual
 * @brief @htmlinclude pkg-marker-qual.html
 * Implementation of the %ListOfTransitions construct from the 'qual' package.
 * 
 * The ListOfTransitions is a container for the Transition elements of a Model.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see Transition
 */
class LIBSBML_EXTERN ListOfTransitions : public ListOf
{

public:

  /**
   * Creates a new ListOfTransitions with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfTransitions
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfTransitions
   *
   * @param pkgVersion an unsigned int, the SBML Qual Version to assign to this ListOfTransitions
   */
  ListOfTransitions(unsigned int level      = QualExtension::getDefaultLevel(),
                    unsigned int version    = QualExtension::getDefaultVersion(),
                    unsigned int pkgVersion = QualExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfTransitions with the given QualPkgNamespaces object.
   *
   * @param qualns the QualPkgNamespaces object
   */
  ListOfTransitions(QualPkgNamespaces* qualns);


  /**
   * Creates and returns a deep copy of this ListOfTransitions object.
   *
   * @return a (deep) copy of this ListOfTransitions object.
   */
  virtual ListOfTransitions* clone () const;


  /**
   * Get a Transition from the ListOfTransitions.
   *
   * @param n the index number of the Transition to get.
   *
   * @return the nth Transition in this ListOfTransitions.
   *
   * @see size()
   */
  virtual Transition* get(unsigned int n);


  /**
   * Get a Transition from the ListOfTransitions.
   *
   * @param n the index number of the Transition to get.
   *
   * @return the nth Transition in this ListOfTransitions.
   *
   * @see size()
   */
  virtual const Transition* get(unsigned int n) const;


  /**
   * Get a Transition from the ListOfTransitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Transition to get.
   *
   * @return Transition in this ListOfTransitions
   * with the given id or NULL if no such
   * Transition exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual Transition* get(const std::string& sid);


  /**
   * Get a Transition from the ListOfTransitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Transition to get.
   *
   * @return Transition in this ListOfTransitions
   * with the given id or NULL if no such
   * Transition exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const Transition* get(const std::string& sid) const;


  /**
   * Removes the nth Transition from this ListOfTransitions
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the Transition to remove.
   *
   * @see size()
   */
  virtual Transition* remove(unsigned int n);


  /**
   * Removes the Transition from this ListOfTransitions with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the Transition to remove.
   *
   * @return the Transition removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual Transition* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfTransitions, is
   * always @c "listOfTransitions".
   *
   * @return the name of this element, i.e. @c "listOfTransitions".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object.
   * 
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for objects contained in this list:
   * @link SBMLTypeCode_t#SBML_QUAL_TRANSITION SBML_QUAL_TRANSITION@endlink (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new Transition in this ListOfTransitions
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Qual package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
Transition_t *
Transition_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion);


LIBSBML_EXTERN
void
Transition_free(Transition_t * t);


LIBSBML_EXTERN
Transition_t *
Transition_clone(Transition_t * t);


LIBSBML_EXTERN
char *
Transition_getId(Transition_t * t);


LIBSBML_EXTERN
char *
Transition_getName(Transition_t * t);


LIBSBML_EXTERN
int
Transition_isSetId(Transition_t * t);


LIBSBML_EXTERN
int
Transition_isSetName(Transition_t * t);


LIBSBML_EXTERN
int
Transition_setId(Transition_t * t, const char * id);


LIBSBML_EXTERN
int
Transition_setName(Transition_t * t, const char * name);


LIBSBML_EXTERN
int
Transition_unsetId(Transition_t * t);


LIBSBML_EXTERN
int
Transition_unsetName(Transition_t * t);


LIBSBML_EXTERN
int
Transition_addInput(Transition_t * t, Input_t * i);


LIBSBML_EXTERN
Input_t *
Transition_createInput(Transition_t * t);


LIBSBML_EXTERN
ListOf_t *
Transition_getListOfInputs(Transition_t * t);


LIBSBML_EXTERN
Input_t *
Transition_getInput(Transition_t * t, unsigned int n);


LIBSBML_EXTERN
Input_t *
Transition_getInputById(Transition_t * t, const char * sid);


LIBSBML_EXTERN
unsigned int
Transition_getNumInputs(Transition_t * t);


LIBSBML_EXTERN
Input_t *
Transition_removeInput(Transition_t * t, unsigned int n);


LIBSBML_EXTERN
Input_t *
Transition_removeInputById(Transition_t * t, const char * sid);


LIBSBML_EXTERN
int
Transition_addOutput(Transition_t * t, Output_t * o);


LIBSBML_EXTERN
Output_t *
Transition_createOutput(Transition_t * t);


LIBSBML_EXTERN
ListOf_t *
Transition_getListOfOutputs(Transition_t * t);


LIBSBML_EXTERN
Output_t *
Transition_getOutput(Transition_t * t, unsigned int n);


LIBSBML_EXTERN
Output_t *
Transition_getOutputById(Transition_t * t, const char * sid);


LIBSBML_EXTERN
unsigned int
Transition_getNumOutputs(Transition_t * t);


LIBSBML_EXTERN
Output_t *
Transition_removeOutput(Transition_t * t, unsigned int n);


LIBSBML_EXTERN
Output_t *
Transition_removeOutputById(Transition_t * t, const char * sid);


LIBSBML_EXTERN
int
Transition_addFunctionTerm(Transition_t * t, FunctionTerm_t * ft);


LIBSBML_EXTERN
FunctionTerm_t *
Transition_createFunctionTerm(Transition_t * t);


LIBSBML_EXTERN
ListOf_t *
Transition_getListOfFunctionTerms(Transition_t * t);


LIBSBML_EXTERN
FunctionTerm_t *
Transition_getFunctionTerm(Transition_t * t, unsigned int n);


LIBSBML_EXTERN
FunctionTerm_t *
Transition_getFunctionTermById(Transition_t * t, const char * sid);


LIBSBML_EXTERN
unsigned int
Transition_getNumFunctionTerms(Transition_t * t);


LIBSBML_EXTERN
FunctionTerm_t *
Transition_removeFunctionTerm(Transition_t * t, unsigned int n);


LIBSBML_EXTERN
FunctionTerm_t *
Transition_removeFunctionTermById(Transition_t * t, const char * sid);


LIBSBML_EXTERN
int
Transition_hasRequiredAttributes(Transition_t * t);


LIBSBML_EXTERN
int
Transition_hasRequiredElements(Transition_t * t);


LIBSBML_EXTERN
Transition_t *
ListOfTransitions_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
Transition_t *
ListOfTransitions_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  Transition_H__  */
