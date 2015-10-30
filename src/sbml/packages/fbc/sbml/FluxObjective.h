/**
 * @file    FluxObjective.h
 * @brief   Definition of FluxObjective, the SBase derived class of the fbc package.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2015 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class FluxObjective
 * @sbmlbrief{fbc} An objective function for a flux.
 *
 * An integral component in a complete description of a steady-state model is
 * the so-called <em>objective function</em>, which generally consists of a
 * linear combination of model variables (fluxes) and a sense (direction). In
 * the SBML Level&nbsp;3 FBC package, this concept is succinctly captured in
 * the Objective class.
 */


#ifndef FluxObjective_H__
#define FluxObjective_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN FluxObjective : public SBase
{

protected:
  /** @cond doxygenLibsbmlInternal */
  std::string   mId;
  std::string   mName;
  std::string   mReaction;
  double        mCoefficient;
  bool          mIsSetCoefficient;
  /** @endcond */

public:

  /**
   * Creates a new FluxObjective with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this FluxObjective
   *
   * @param version an unsigned int, the SBML Version to assign to this FluxObjective
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this FluxObjective
   */
  FluxObjective(unsigned int level      = FbcExtension::getDefaultLevel(),
                unsigned int version    = FbcExtension::getDefaultVersion(),
                unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new FluxObjective with the given FbcPkgNamespaces object.
   *
   * @param fbcns the FbcPkgNamespaces object
   */
  FluxObjective(FbcPkgNamespaces* fbcns);


   /**
   * Copy constructor for FluxObjective.
   *
   * @param orig; the FluxObjective instance to copy.
   */
  FluxObjective(const FluxObjective& orig);


   /**
   * Assignment operator for FluxObjective.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  FluxObjective& operator=(const FluxObjective& rhs);


   /**
   * Creates and returns a deep copy of this FluxObjective object.
   *
   * @return a (deep) copy of this FluxObjective object.
   */
  virtual FluxObjective* clone () const;


   /**
   * Destructor for FluxObjective.
   */
  virtual ~FluxObjective();


   /**
   * Returns the value of the "id" attribute of this FluxObjective.
   *
   * @return the value of the "id" attribute of this FluxObjective as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this FluxObjective.
   *
   * @return the value of the "name" attribute of this FluxObjective as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "reaction" attribute of this FluxObjective.
   *
   * @return the value of the "reaction" attribute of this FluxObjective as a string.
   */
  virtual const std::string& getReaction() const;


  /**
   * Returns the value of the "coefficient" attribute of this FluxObjective.
   *
   * @return the value of the "coefficient" attribute of this FluxObjective as a double.
   */
  virtual double getCoefficient() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective's "id" attribute has been set.
   *
   * @return @c true if this FluxObjective's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective's "name" attribute has been set.
   *
   * @return @c true if this FluxObjective's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective's "reaction" attribute has been set.
   *
   * @return @c true if this FluxObjective's "reaction" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetReaction() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective's "coefficient" attribute has been set.
   *
   * @return @c true if this FluxObjective's "coefficient" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoefficient() const;


  /**
   * Sets the value of the "id" attribute of this FluxObjective.
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
   * Sets the value of the "name" attribute of this FluxObjective.
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
   * Sets the value of the "reaction" attribute of this FluxObjective.
   *
   * @param reaction; const std::string& value of the "reaction" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setReaction(const std::string& reaction);


  /**
   * Sets the value of the "coefficient" attribute of this FluxObjective.
   *
   * @param coefficient; double value of the "coefficient" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoefficient(double coefficient);


  /**
   * Unsets the value of the "id" attribute of this FluxObjective.
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
   * Unsets the value of the "name" attribute of this FluxObjective.
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
   * Unsets the value of the "reaction" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetReaction();


  /**
   * Unsets the value of the "coefficient" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoefficient();


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns the XML element name of this object, which for FluxObjective, is
   * always @c "fluxObjective".
   *
   * @return the name of this element, i.e. @c "fluxObjective".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this FluxObjective object have been set.
   *
   * @note The required attributes for a FluxObjective object are:
   * @li "reaction"
   * @li "coefficient"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


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
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

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
 * @class ListOfFluxObjectives
 * @sbmlbrief{fbc} A list of FluxObjective objects.
 * 
 * The ListOfFluxObjectives is a container for the &ldquo;fbc&rdquo;
 * Objective that indicate which fluxes (and what ratios for those fluxes)
 * are to be used in maximizing or minimizing the Objective.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see Objective
 * @see FluxObjective
 */
class LIBSBML_EXTERN ListOfFluxObjectives : public ListOf
{

public:

  /**
   * Creates a new ListOfFluxObjectives with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfFluxObjectives
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfFluxObjectives
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this ListOfFluxObjectives
   */
  ListOfFluxObjectives(unsigned int level      = FbcExtension::getDefaultLevel(),
                       unsigned int version    = FbcExtension::getDefaultVersion(),
                       unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfFluxObjectives with the given FbcPkgNamespaces object.
   *
   * @param fbcns the FbcPkgNamespaces object
   */
  ListOfFluxObjectives(FbcPkgNamespaces* fbcns);


   /**
   * Creates and returns a deep copy of this ListOfFluxObjectives object.
   *
   * @return a (deep) copy of this ListOfFluxObjectives object.
   */
  virtual ListOfFluxObjectives* clone () const;


   /**
   * Get a FluxObjective from the ListOfFluxObjectives.
   *
   * @param n the index number of the FluxObjective to get.
   *
   * @return the nth FluxObjective in this ListOfFluxObjectives.
   *
   * @see size()
   */
	virtual FluxObjective* get(unsigned int n);


  /**
   * Get a FluxObjective from the ListOfFluxObjectives.
   *
   * @param n the index number of the FluxObjective to get.
   *
   * @return the nth FluxObjective in this ListOfFluxObjectives.
   *
   * @see size()
   */
	virtual const FluxObjective* get(unsigned int n) const;


  /**
   * Get a FluxObjective from the ListOfFluxObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FluxObjective to get.
   *
   * @return FluxObjective in this ListOfFluxObjectives
   * with the given id or NULL if no such
   * FluxObjective exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual FluxObjective* get(const std::string& sid);


  /**
   * Get a FluxObjective from the ListOfFluxObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FluxObjective to get.
   *
   * @return FluxObjective in this ListOfFluxObjectives
   * with the given id or NULL if no such
   * FluxObjective exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const FluxObjective* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "FluxObjective" to this ListOfFluxObjectives.
	 *
	 * @param fo; the FluxObjective object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addFluxObjective(const FluxObjective* fo);


	/**
	 * Get the number of FluxObjective objects in this ListOfFluxObjectives.
	 *
	 * @return the number of FluxObjective objects in this ListOfFluxObjectives
	 */
	unsigned int getNumFluxObjectives() const;


	/**
	 * Creates a new FluxObjective object, adds it to the
	 * ListOfFluxObjectives and returns the FluxObjective object created. 
	 *
	 * @return a new FluxObjective object instance
	 *
	 * @see addFluxObjective(const FluxObjective* fo)
	 */
	FluxObjective* createFluxObjective();


  /**
   * Removes the nth FluxObjective from this ListOfFluxObjectives
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the FluxObjective to remove.
   *
   * @see size()
   */
	virtual FluxObjective* remove(unsigned int n);


  /**
   * Removes the FluxObjective from this ListOfFluxObjectives with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the FluxObjective to remove.
   *
   * @return the FluxObjective removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual FluxObjective* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfFluxObjectives, is
   * always @c "listOfFluxObjectives".
   *
   * @return the name of this element, i.e. @c "listOfFluxObjectives".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for the objects in this ListOf instance, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new FluxObjective in this ListOfFluxObjectives
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Fbc package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new FluxObjective_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * FluxObjective_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * FluxObjective_t structure.
 *
 * @returns the newly-created FluxObjective_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
FluxObjective_t *
FluxObjective_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion);


/**
 * Frees the given FluxObjective_t structure.
 * 
 * @param fo the FluxObjective_t structure to be freed.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
void
FluxObjective_free(FluxObjective_t * fo);


/**
 * Creates a deep copy of the given FluxObjective_t structure.
 * 
 * @param fo the FluxObjective_t structure to be copied.
 *
 * @returns a (deep) copy of the given FluxObjective_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
FluxObjective_t *
FluxObjective_clone(FluxObjective_t * fo);


/**
 * Returns the value of the "id" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the id of this structure.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
const char *
FluxObjective_getId(const FluxObjective_t * fo);


/**
 * Returns the value of the "name" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the name of this structure.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
const char *
FluxObjective_getName(const FluxObjective_t * fo);


/**
 * Returns the value of the "reaction" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the reaction of this structure.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
const char *
FluxObjective_getReaction(const FluxObjective_t * fo);


/**
 * Returns the value of the "coefficient" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the coefficient of this structure.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
double
FluxObjective_getCoefficient(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 if the given FluxObjective_t structure's "id"
 * is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 if the "id" of this FluxObjective_t structure is
 * set, @c 0 otherwise.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetId(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 if the given FluxObjective_t structure's "name"
 * is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 if the "name" of this FluxObjective_t structure is
 * set, @c 0 otherwise.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetName(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 if the given FluxObjective_t structure's "reaction"
 * is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 if the "reaction" of this FluxObjective_t structure is
 * set, @c 0 otherwise.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetReaction(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 if the given FluxObjective_t structure's "coefficient"
 * is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 if the "coefficient" of this FluxObjective_t structure is
 * set, @c 0 otherwise.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetCoefficient(const FluxObjective_t * fo);


/**
 * Sets the "id" attribute of the given FluxObjective_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs FluxObjective_unsetId() instead.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param id the string to which the structures "id" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setId(FluxObjective_t * fo, const char * id);


/**
 * Sets the "name" attribute of the given FluxObjective_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs FluxObjective_unsetName() instead.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param name the string to which the structures "name" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setName(FluxObjective_t * fo, const char * name);


/**
 * Sets the "reaction" attribute of the given FluxObjective_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs FluxObjective_unsetReaction() instead.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param reaction the string to which the structures "reaction" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setReaction(FluxObjective_t * fo, const char * reaction);


/**
 * Sets the "coefficient" attribute of the given FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param coefficient the string to which the structures "coefficient" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setCoefficient(FluxObjective_t * fo, double coefficient);


/**
 * Unsets the value of the "id" attribute of the given 
 * FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetId(FluxObjective_t * fo);


/**
 * Unsets the value of the "name" attribute of the given 
 * FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetName(FluxObjective_t * fo);


/**
 * Unsets the value of the "reaction" attribute of the given 
 * FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetReaction(FluxObjective_t * fo);


/**
 * Unsets the value of the "coefficient" attribute of the given 
 * FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetCoefficient(FluxObjective_t * fo);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given FluxObjective_t structure have been set.
 *
 * @param fo the FluxObjective_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_hasRequiredAttributes(const FluxObjective_t * fo);


LIBSBML_EXTERN
FluxObjective_t *
ListOfFluxObjectives_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
FluxObjective_t *
ListOfFluxObjectives_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  FluxObjective_H__  */

