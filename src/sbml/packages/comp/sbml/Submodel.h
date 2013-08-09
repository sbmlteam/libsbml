/**
 * @file    Submodel.h
 * @brief   Definition of Submodel, the SBase derived class of the comp package.
 * @author  Lucian Smith 
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2011 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class Submodel
 * @ingroup Comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %Submodel construct from the 'comp' package.
 *
 * The Submodel class was introduced by the SBML Level&nbsp;3 
 * @ref Comp "Hierarchical Model Composition" package ('comp')
 * as the principle way by which models
 * are structured hierarchically.  Submodels are instantiations of models
 * contained within other models.  They reference another Model that is to be
 * instantiated within its parent Model, and additionally define how that
 * Model is to be modified before instantiation.
 *
 * The Submodel object class has a required attribute "modelRef", which must
 * reference another Model or ExternalModelDefinition object present in the
 * SBML Document.  This referenced Model is the model to be instantiated.
 * 
 * It also has a required attribute, "id", to give the submodel a unique
 * identifier by which other parts of an SBML model definition can refer to
 * it, and an optional "name" attribute of type @c string.  Identifiers and
 * names must be used according to the guidelines described in the SBML
 * specification.
 *
 * The Submodel class also provides constructs that define how the referenced
 * Model object is to be modified before it is instantiated in the enclosing
 * model.  If numerical values in the referenced model must be changed in order 
 * to fit them into their new context as part of the submodel, the changes can 
 * be handled through conversion factors.  If one or more structural features 
 * in the referenced model are undesirable and should be removed, the changes 
 * can be handled through deletions.  (For example, an initial assignment or 
 * reaction may not be relevant in its new context and should be removed.)
 *
 * In some cases, the referenced Model may have been written with different
 * units than the containing model.  For most model elements, this is not a
 * problem: it is already possible to have Species and Parameter objects with
 * different units in a single model, for example, so in this case the
 * resulting hierarchical model would be treated in exactly the same way as
 * any other model with Species and Parameters with different units.
 *
 * However, two units in SBML models are fixed and must not vary between SBML
 * elements: time and extent.  The units of time are set once per model, and
 * affect the core elements of RateRule, KineticLaw, Delay, and the
 * csymbols 'time' and 'delay'.  Even if the model does not explicitly state
 * what the units of time actually are, they are defined to be consistent
 * across the model, and therefore might differ from the units of time across
 * a parent model.  To correct this imbalance, the optional attribute
 * "timeConversionFactor" may be used, which, if defined, must reference a
 * constant parameter in the parent model. The value of the time conversion
 * factor should be defined such that a single unit of time in the Submodel
 * multiplied by the time conversion factor should equal a single unit of
 * time in the parent model.
 *
 * Extent is the unit in SBML that defines how the KineticLaw of a Reaction
 * affects species quantities: kinetic laws are defined to be in units of
 * extent/time.  No other SBML core construct is defined in terms of extent.
 * If the effective units of extent in a submodel differ from the effective
 * units of extent in the parent model (regardless of whether either defined
 * what those units actually are), the optional attribute
 * "extentConversionFactor" may be used, which, if defined, must reference a
 * constant parameter in the parent model. The value of the extent conversion
 * factor should be defined such that a single unit of extent in the Submodel
 * multiplied by the extent conversion factor should equal a single unit of
 * extent in the parent model.
 *
 * If features of the referenced model must be removed, a Deletion should be added 
 * to the Submodel object.  A Submodel may contain a child ListOfDeletions, which
 * in turn may contain one or more Deletion items.  Each Deletion references a single
 * element of the referenced Model that must be removed before instantiating that
 * Model as a submodel of the parent Model.
 */

#ifndef Submodel_H__
#define Submodel_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/CompBase.h>
#include <sbml/packages/comp/sbml/ListOfDeletions.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class ReplacedElement;

class LIBSBML_EXTERN Submodel : public CompBase
{
protected:

  /** @cond doxygenLibsbmlInternal */
  std::string   mId;
  std::string   mName;
  std::string   mModelRef;
  std::string   mSubstanceConversionFactor;
  std::string   mTimeConversionFactor;
  std::string   mExtentConversionFactor;
  ListOfDeletions  mListOfDeletions;
  Model*        mInstantiatedModel;
  std::string   mInstantiationOriginalURI;
  /** @endcond */

public:

  /**
   * Creates a new Submodel with the given level, version, and package
   * version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  Submodel(unsigned int level      = CompExtension::getDefaultLevel(),
           unsigned int version    = CompExtension::getDefaultVersion(),
           unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new Submodel with the given CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  Submodel(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   */
  Submodel(const Submodel& source);


  /**
   * Assignment operator.
   */
  Submodel& operator=(const Submodel& source);


  /**
   * Creates and returns a deep copy of this Submodel object.
   * 
   * @return a (deep) copy of this Submodel object
   */
  virtual Submodel* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~Submodel ();


  /**
   * Returns the first child element found that has the given @p id in the
   * model-wide SId namespace, or @c NULL if no such object is found.
   *
   * @param id string representing the id of objects to find
   *
   * @return a pointer to the SBase element with the given @p id.
   */
  virtual SBase* getElementBySId(std::string id);
  
  
  /**
   * Returns the first child element it can find with the given @p metaid, or
   * itself if it has the given @p metaid, or @c NULL if no such object is found.
   *
   * @param metaid string representing the metaid of objects to find
   *
   * @return a pointer to the SBase element with the given @p metaid.
   */
  virtual SBase* getElementByMetaId(std::string metaid);
  
  
  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List of pointers to all children objects.
   */
  virtual List* getAllElements(ElementFilter* filter=NULL);
  
  
  /**
   * Returns the value of the "id" attribute of this Submodel.
   *
   * @return the value of the "id" attribute of this Submodel.
   */
  virtual const std::string& getId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Submodel's "id" attribute has been set.
   *
   * @return @c true if this Submodel's "id" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;

  
  /**
   * Sets the value of the "id" attribute of this Submodel.  Fails if the id
   * is not a valid syntax for an SId.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this Submodel.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this Submodel.
   *
   * @return the value of the "name" attribute of this Submodel.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Submodel's "name" attribute has been set.
   *
   * @return @c true if this Submodel's "name" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;

  
  /**
   * Sets the value of the "name" attribute of this Submodel.  Fails if the
   * name is empty.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this Submodel.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetName();


  /**
   * Returns the value of the "modelRef" attribute of this Submodel.
   *
   * @return the value of the "modelRef" attribute of this Submodel.
   */
  virtual const std::string& getModelRef() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Submodel's "modelRef" attribute has been set.
   *
   * @return @c true if this Submodel's "modelRef" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetModelRef() const;

  
  /**
   * Sets the value of the "modelRef" attribute of this Submodel.  Fails if
   * the modelRef is not a valid syntax for an SIdRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setModelRef(const std::string& modelRef);


  /**
   * Unsets the value of the "modelRef" attribute of this Submodel.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetModelRef();


  /**
   * Returns the value of the "substanceConversionFactor" attribute of this
   * Submodel.
   *
   * @return the value of the "substanceConversionFactor" attribute of this Submodel.
   */
  virtual const std::string& getSubstanceConversionFactor () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Submodel's "substanceConversionFactor" attribute has been set.
   *
   * @return @c true if this Submodel's "substanceConversionFactor" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSubstanceConversionFactor () const;

  
  /**
   * Sets the value of the "substanceConversionFactor" attribute of this
   * Submodel.  Fails if the id is not a valid syntax for an SIdRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setSubstanceConversionFactor (const std::string& id);


  /**
   * Unsets the value of the "substanceConversionFactor" attribute of this Submodel.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetSubstanceConversionFactor ();


  /**
   * Returns the value of the "timeConversionFactor" attribute of this Submodel.
   *
   * @return the value of the "timeConversionFactor" attribute of this Submodel.
   */
  virtual const std::string& getTimeConversionFactor () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Submodel's "timeConversionFactor" attribute has been set.
   *
   * @return @c true if this Submodel's "timeConversionFactor" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetTimeConversionFactor () const;

  
  /**
   * Sets the value of the "timeConversionFactor" attribute of this Submodel.
   * Fails if the id is not a valid syntax for an SIdRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setTimeConversionFactor (const std::string& id);


  /**
   * Unsets the value of the "timeConversionFactor" attribute of this Submodel.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetTimeConversionFactor ();


  /**
   * Returns the value of the "extentConversionFactor" attribute of this Submodel.
   *
   * @return the value of the "extentConversionFactor" attribute of this Submodel.
   */
  virtual const std::string& getExtentConversionFactor () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Submodel's "extentConversionFactor" attribute has been set.
   *
   * @return @c true if this Submodel's "extentConversionFactor" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetExtentConversionFactor () const;

  
  /**
   * Sets the value of the "extentConversionFactor" attribute of this
   * Submodel.  Fails if the id is not a valid syntax for an SIdRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setExtentConversionFactor (const std::string& id);


  /**
   * Unsets the value of the "extentConversionFactor" attribute of this
   * Submodel.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetExtentConversionFactor ();


  /**
   * Returns the ListOf object that holds all deletions.
   *
   * @return the ListOf object that holds all deletions.
   */ 
  const ListOfDeletions* getListOfDeletions () const;


  /**
   * Returns the ListOf object that holds all deletions.
   *
   * @return the ListOf object that holds all deletions.
   */ 
  ListOfDeletions* getListOfDeletions ();


  /**
   * Returns the deletion with the given index.
   * If the index is invalid, @c NULL is returned.
   *
   * @param n the index number of the Deletion to get.
   *
   * @return the nth Deletion in the ListOfDeletions.
   */ 
  Deletion* getDeletion (unsigned int n);


  /**
   * Returns the deletion with the given index.
   * If the index is invalid, @c NULL is returned.
   *
   * @param n the index number of the Deletion to get.
   *
   * @return the nth Deletion in the ListOfDeletions.
   */ 
  const Deletion* getDeletion (unsigned int n) const;


  /**
   * Returns the deletion with the given @p id.
   * If the id is invalid, @c NULL is returned.
   *
   * @param id the id of the Deletion to get.
   *
   * @return the Deletion in the ListOfDeletions with the given @p id.
   */ 
  Deletion* getDeletion (std::string id);


  /**
   * Returns the deletion with the given @p id.
   * If the id is invalid, @c NULL is returned.
   *
   * @param id the id of the Deletion to get.
   *
   * @return the Deletion in the ListOfDeletions with the given @p id.
   */ 
  const Deletion* getDeletion (std::string id) const;


  /**
   * Adds a copy of the given Deletion object to the list of deletions.
   *
   * @param deletion the Deletion object to be added to the list of
   * deletions.  Fails if the added deletion is NULL, does not match the
   * level/version/package of the parent object, or cannot be added to the
   * list of deletions.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_LEVEL_MISMATCH LIBSBML_LEVEL_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_VERSION_MISMATCH LIBSBML_VERSION_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_PKG_VERSION_MISMATCH LIBSBML_VERSION_MISMATCH @endlink
   */
  int addDeletion (const Deletion* deletion);


  /**
   * Returns the number of deletions for this Submodel.
   *
   * @return the number of deletions for this Submodel.
   */
  unsigned int getNumDeletions () const;


  /**
   * Creates a Deletion object, adds it to the end of the
   * deletion objects list and returns a pointer to the newly
   * created object.
   *
   * @return a newly created Deletion object
   */
  Deletion* createDeletion ();


  /**
   * Removes the deletion with the given index from the Submodel.
   * A pointer to the deletion that was removed is returned.
   * If no deletion has been removed, @c NULL is returned.
   *
   * @param index the index of the Deletion object to remove
   *
   * @return the Deletion object removed.  As mentioned above, 
   * the caller owns the returned object. @c NULL is returned if 
   * the given index is out of range.
   */
  Deletion* removeDeletion(unsigned int index);


  /**
   * Removes the deletion with the given identifier from the Submodel.
   * A pointer to the deletion that was removed is returned.
   * If no deletion has been removed, @c NULL is returned.
   *
   * @param sid string representing the identifier
   * of the Deletion object to remove
   *
   * @return the Deletion object removed.  As mentioned above, 
   * the caller owns the returned object. @c NULL is returned if 
   * the given @p sid is not found.
   */
  Deletion* removeDeletion(const std::string& sid);


  /**
   * Returns true if the 'submodel' attribute is set, and if getNumReferents() is exactly 1.
   *
   * @return boolean: 'true' if the attributes are correctly set; 'false' if not.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * Renames the conversion factor attributes on this element if @p oldid matches.
   */
  virtual void renameSIdRefs(std::string oldid, std::string newid);


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * LibSBML attaches an identifying code to every kind of SBML object.
   * These are known as <em>SBML type codes</em>.  @if clike The set of
   * possible type codes for the 'comp' package is defined in the enumeration
   * #SBMLCompTypeCode_t.  The names of the type codes all begin with the
   * characters <code>SBML_COMP</code>. @endif@~
   * 
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactants.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument object to use
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets this SBML object to child SBML objects (if any).
   * (Creates a child-parent relationship by the parent)
   *
   * Subclasses must override this function if they define
   * one ore more child elements.
   * Basically, this function needs to be called in
   * constructor, copy constructor, assignment operator.
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  virtual void connectToChild ();
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */


  /**
   * Find and create a local copy of the Model object referenced by this
   * Submodel.  Is recursive, in that if the instantiated Model contains any
   * Submodel objects, those Submodels will themselves be instantiated.  If
   * an instantiated model previously existed, it is deleted and a new one is
   * created.  For this reason, call this function only once, or 
   * call Submodel::getInstantiation().
   *
   * @return an integer value indicating success/failure of the operation.
   * Possible return values from this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * In this case, 'invalid object' means that this Submodel itself is invalid, and no Model can be instantiated from it.
   */
  virtual int instantiate();


  /**
   * Delete elements in the instantiated submodel, based on any Deletions
   * from this Submodel's listOfDeletions.
   *
   * @return an integer value indicating success/failure of the operation.
   * Possible return values from this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * In this case, 'invalid object' means that this Submodel itself is invalid, and no Model can be instantiated from it.
   */
  virtual int performDeletions();


  /**
   * Delete the element in question from the stored instantiated Model, and
   * replace all references to it with references to the replacement object.
   * @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * means that this Submodel itself or one of the passed-in objects are invalid.
   * @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * means that the routine failed for some othe reason.
   *
   * @return an integer value indicating success/failure of the operation.
   * Possible return values from this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int replaceElement(SBase* toReplace, SBase* replacement);


  /**
   * Get the instantiated Model this Submodel contains rules to create.
   * Calls instantiate() automatically if this operation has not yet been
   * performed, and/or if the operation failed the last time it was called.
   * Any modifictions that have been performed with performDeletions(), 
   * replaceElement(), or convertTimeAndExtent() function calls will be included.
   *
   * @return the instantiated Model object: a clone of the original, modified
   * according to the performDeletions() and replaceElement() functions that
   * have been called.  Returns NULL if any error is encountered.
   */
  virtual Model* getInstantiation();


  /**
   * Get the instantiated Model this Submodel contains rules to create.
   * Calls instantiate() automatically if this operation has not yet been
   * performed, and/or if the operation failed the last time it was called.
   * Any modifictions that have been performed with performDeletions(), 
   * replaceElement(), or convertTimeAndExtent() function calls will be included.
   *
   * @return the instantiated Model object: a clone of the original, modified
   * according to the performDeletions() and replaceElement() functions that
   * have been called.  Returns NULL if any error is encountered.
   */
  virtual const Model* getInstantiation() const;


  /**
   * Delete the instantiated Model, if it exists.
   */
  virtual void clearInstantiation();

  
  /**
   * Get all instantiated sub-elements, including any elements from
   * instantiated submodels, etc.
   */
  virtual List* getAllInstantiatedElements();


  /**
   * Convert all references to time and extent in the instantiated
   * Model, according to the
   * timeConversionFactor and extentConversionFactor attributes.
   */
  virtual int convertTimeAndExtent();

protected:

  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "submodel", mId );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */

private:
  /**
   * Internal function to convert time and extent with the given ASTNodes.
   */
  virtual int convertTimeAndExtentWith(const ASTNode* time, const ASTNode* xcf, const ASTNode* klmod);

  /**
   * Internal function that changes 'math' according to the passed-in time conversion factors (pre-set-up for convenience)
   */
  virtual void convertCSymbols(ASTNode*& math, const ASTNode* tcfdiv, const ASTNode* tcftimes);

  /**
   * Internal function that creates a new conversion factor in the given Model* based on newcf and oldcf, and sets 'cf' to be the name of that new conversion factor.
   */
  virtual void createNewConversionFactor(std::string& cf, const ASTNode* newcf, std::string oldcf, Model* model);
};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
Submodel_t *
Submodel_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion);


LIBSBML_EXTERN
void
Submodel_free(Submodel_t * s);


LIBSBML_EXTERN
Submodel_t *
Submodel_clone(Submodel_t * s);


LIBSBML_EXTERN
char *
Submodel_getId(Submodel_t * s);


LIBSBML_EXTERN
char *
Submodel_getName(Submodel_t * s);


LIBSBML_EXTERN
char *
Submodel_getModelRef(Submodel_t * s);


LIBSBML_EXTERN
char *
Submodel_getTimeConversionFactor(Submodel_t * s);


LIBSBML_EXTERN
char *
Submodel_getExtentConversionFactor(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_isSetId(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_isSetName(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_isSetModelRef(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_isSetTimeConversionFactor(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_isSetExtentConversionFactor(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_setId(Submodel_t * s, const char * id);


LIBSBML_EXTERN
int
Submodel_setName(Submodel_t * s, const char * name);


LIBSBML_EXTERN
int
Submodel_setModelRef(Submodel_t * s, const char * modelRef);


LIBSBML_EXTERN
int
Submodel_setTimeConversionFactor(Submodel_t * s, const char * timeConversionFactor);


LIBSBML_EXTERN
int
Submodel_setExtentConversionFactor(Submodel_t * s, const char * extentConversionFactor);


LIBSBML_EXTERN
int
Submodel_unsetId(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_unsetName(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_unsetModelRef(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_unsetTimeConversionFactor(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_unsetExtentConversionFactor(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_addDeletion(Submodel_t * s, Deletion_t * d);


LIBSBML_EXTERN
Deletion_t *
Submodel_createDeletion(Submodel_t * s);


LIBSBML_EXTERN
ListOf_t *
Submodel_getListOfDeletions(Submodel_t * s) ;


LIBSBML_EXTERN
Deletion_t *
Submodel_getDeletion(Submodel_t * s, unsigned int n);


LIBSBML_EXTERN
Deletion_t *
Submodel_getDeletionById(Submodel_t * s, const char * sid);


LIBSBML_EXTERN
unsigned int
Submodel_getNumDeletions(Submodel_t * s);


LIBSBML_EXTERN
Deletion_t *
Submodel_removeDeletion(Submodel_t * s, unsigned int n);


LIBSBML_EXTERN
Deletion_t *
Submodel_removeDeletionById(Submodel_t * s, const char * sid);


LIBSBML_EXTERN
int
Submodel_hasRequiredAttributes(Submodel_t * s);


LIBSBML_EXTERN
int
Submodel_hasRequiredElements(Submodel_t * s);


LIBSBML_EXTERN
Submodel_t *
ListOfSubmodels_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
Submodel_t *
ListOfSubmodels_removeById(ListOf_t * lo, const char * sid);



END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Submodel_H__ */
