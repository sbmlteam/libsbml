/**
 * @file    Objective.h
 * @brief   Definition of Objective, the SBase derived class of the fbc package.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009-2013 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class Objective
 * @ingroup fbc
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the 'fbc' package %Objective construct.
 *
 * The FBC Objective class is derived from SBML SBase and inherits metaid and sboTerm, as well as the subcomponents for Annotation and Notes. An integral component in a complete description of a steady-state model is the so-called 'objective function' which generally consists of a linear combination of model variables (fluxes) and a sense (direction). In the FBC package this concept is succinctly captured in the Objective class.
 */


#ifndef Objective_H__
#define Objective_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>

LIBSBML_CPP_NAMESPACE_BEGIN

typedef enum
{
    OBJECTIVE_TYPE_MAXIMIZE
  , OBJECTIVE_TYPE_MINIMIZE
  , OBJECTIVE_TYPE_UNKNOWN
} ObjectiveType_t;

LIBSBML_CPP_NAMESPACE_END


#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/FluxObjective.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Objective : public SBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string mId;
  std::string mName;
  ObjectiveType_t mType;
  std::string mTypeString;
  ListOfFluxObjectives mFluxes;
  bool mIsSetListOfFluxObjectives;
  /** @endcond */


public:

  /**
   * Creates a new Objective with the given level, version, and package version.
   */
   Objective(unsigned int level      = FbcExtension::getDefaultLevel(),
          unsigned int version    = FbcExtension::getDefaultVersion(),
          unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new Objective with the given FbcPkgNamespaces object.
   */
   Objective(FbcPkgNamespaces* fbcns);


  /**
   * Copy constructor.
   */
   Objective(const Objective& source);


  /**
   * Assignment operator.
   */
   Objective& operator=(const Objective& source);


  /**
   * Destructor.
   */ 
  virtual ~Objective ();

  
  
  /**
   * Returns the first child element found that has the given @p id in the model-wide SId namespace, or @c NULL if no such object is found.
   *
   * @param id string representing the id of objects to find
   *
   * @return a pointer to the SBase element with the given @p id.
   */
  virtual SBase* getElementBySId(const std::string& id);
  
  
  /**
   * Returns the first child element it can find with the given @p metaid, or itself if it has the given @p metaid, or @c NULL if no such object is found.
   *
   * @param metaid string representing the metaid of objects to find
   *
   * @return a pointer to the SBase element with the given @p metaid.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);
  
  /**
   * Returns a List of all child SBase* objects, including those nested to an arbitrary depth
   *
   * @return a List* of pointers to all children objects.
   */
  virtual List* getAllElements(ElementFilter* filter=NULL);
  
  
  /**
   * Returns the value of the "name" attribute of this Objective.
   *
   * @return the value of the "name" attribute of this Objective.
   */
  virtual const std::string& getName () const;
  
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * Objective's "name" attribute has been set.
   *
   * @return @c true if this Objective's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName () const;
  
  
  /**
   * Sets the value of the "name" attribute of this Objective.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setName (const std::string& name);
  
  
  /**
   * Unsets the value of the "name" attribute of this Objective.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetName ();


  /**
   * Returns the string of the "type" attribute of this Objective.
   *
   * @return the string of the "type" attribute of this Objective.
   */
  const std::string& getType ();


  /**
   * Returns the ObjectiveType_t of the "type" attribute of this Objective.
   *
   * @return the ObjectiveType_t of the "type" attribute of this Objective.
   */
  ObjectiveType_t getObjectiveType () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Objective's "type" attribute has been set.
   *
   * @return @c true if this Objective's "type" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetType () const;

  
  /**
   * Sets the SIdRef string of the "type" attribute of this Objective.
   *
   * @param type a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType (const std::string& type);


  /**
   * Sets the SIdRef string of the "type" attribute of this Objective.
   *
   * @param type a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setType (ObjectiveType_t type);


  /**
   * Unsets the value of the "id" attribute of this Objective.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetType ();

  
  /**
   * Returns the string of the "id" attribute of this Objective.
   *
   * @return the string of the "id" attribute of this Objective.
   */
  virtual const std::string& getId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Objective's "id" attribute has been set.
   *
   * @return @c true if this Objective's "id" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetId () const;

  
  /**
   * Sets the SIdRef string of the "id" attribute of this Objective.
   *
   * @param id a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setId (const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this Objective.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetId ();

  
  /**
   * Returns the ListOf object that holds all members.
   *
   * @return the ListOf object that holds all members.
   */ 
  const ListOfFluxObjectives* getListOfFluxObjectives () const;

  /**
   * Returns the member with the given index.
   * If the index is invalid, @c NULL is returned.
   *
   * @param n the index number of the FluxObjective to get.
   *
   * @return the nth FluxObjective in the ListOfFluxObjectives.
   */ 
  FluxObjective* getFluxObjective (unsigned int n);

  /**
   * Returns the member with the given index.
   * If the index is invalid, @c NULL is returned.
   *
   * @param n the index number of the FluxObjective to get.
   *
   * @return the nth FluxObjective in the ListOfFluxObjectives.
   */ 
  const FluxObjective* getFluxObjective (unsigned int n) const;

  /**
   * Returns the member with the given symbol.
   * If the index is invalid, @c NULL is returned.
   *
   * @param symbol a string representing the symbol attribute
   * of the FluxObjective to get.
   * 
   * @return FluxObjective in the ListOfFluxObjectives with the given symbol
   * or NULL if no such FluxObjective exists.
   */
  FluxObjective* getFluxObjective (const std::string& symbol);


  /**
   * Returns the member with the given symbol.
   * If the index is invalid, @c NULL is returned.
   *
   * @param symbol a string representing the symbol attribute
   * of the FluxObjective to get.
   * 
   * @return FluxObjective in the ListOfFluxObjectives with the given symbol
   * or NULL if no such FluxObjective exists.
   */
  const FluxObjective* getFluxObjective (const std::string& symbol) const;


  /**
   * Adds a copy of the given FluxObjective object to the list of members.
   *
   * @param member the FluxObjective object to be added to the list of 
   * members.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addFluxObjective (const FluxObjective* member);


  /**
   * Returns the number of members for this objective.
   *
   * @return the number of members for this objective.
   */
  unsigned int getNumFluxObjectives () const;


  /**
   * Creates a FluxObjective object, adds it to the end of the
   * member objects list and returns a pointer to the newly
   * created object.
   *
   * @return a newly created FluxObjective object
   */
  FluxObjective* createFluxObjective ();


  /**
   * Removes the member with the given index from the objective.
   * A pointer to the member that was removed is returned.
   * If no member has been removed, @c NULL is returned.
   *
   * @param index the index of the FluxObjective object to remove
   *
   * @return the FluxObjective object removed.  As mentioned above, 
   * the caller owns the returned object. @c NULL is returned if 
   * the given index is out of range.
   */
  FluxObjective* removeFluxObjective(unsigned int index);


  /**
   * Removes the member with the given symbol from the objective.
   * A pointer to the member that was removed is returned.
   * If no member has been removed, @c NULL is returned.
   *
   * @param symbol the symbol attribute of the FluxObjective object to remove
   *
   * @return the FluxObjective object removed.  As mentioned above, 
   * the caller owns the returned object. @c NULL is returned if 
   * the given index is out of range.
   */
  FluxObjective* removeFluxObjective(const std::string& symbol);

  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * Creates and returns a deep copy of this Objective object.
   *
   * @return a (deep) copy of this Objective.
   */
  virtual Objective* clone () const;


  /**
   * Returns the libSBML type code of this object instance.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @link SBMLFbcTypeCode_t#SBML_FBC_OBJECTIVE SBML_FBC_OBJECTIVE@endlink
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


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


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  
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
   * Enables/Disables the given package with this element and child
   * elements (if any).
   * (This is an internal implementation for enablePakcage function)
   *
   * @note Subclasses in which one or more child elements are defined
   * must override this function.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /* function returns true if component has all the required
   * elements
   * needs to be overloaded for each component
   */
  virtual bool hasRequiredElements() const ;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  bool getIsSetListOfFluxObjectives() const;
  /** @endcond */
    
protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * Create and return an SBML object of this class, if present.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
  createObject (XMLInputStream& stream);
  /** @endcond */


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
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */
};

/**
 * @class ListOfObjectives
 * @ingroup fbc
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the %ListOfObjectives construct from the 'fbc' package.
 * 
 * The ListOfObjectives is a container for the Objective elements of Model.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see Objective
 */
class LIBSBML_EXTERN ListOfObjectives : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfObjectives object.
   *
   * @return a (deep) copy of this ListOfObjectives.
   */
  virtual ListOfObjectives* clone () const;


  /**
   * Creates a new ListOfObjectives with the given level, version, and package version.
   */
   ListOfObjectives(unsigned int level      = FbcExtension::getDefaultLevel(), 
                 unsigned int version    = FbcExtension::getDefaultVersion(), 
                 unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfObjectives with the given FbcPkgNamespaces object.
   */
   ListOfObjectives(FbcPkgNamespaces* fbcns);


  /**
   * Get a Objective from the ListOfObjectives.
   *
   * @param n the index number of the Objective to get.
   * 
   * @return the nth Objective in this ListOfObjectives.
   *
   * @see size()
   */
  virtual Objective * get(unsigned int n); 


  /**
   * Get a Objective from the ListOfObjectives.
   *
   * @param n the index number of the Objective to get.
   * 
   * @return the nth Objective in this ListOfObjectives.
   *
   * @see size()
   */
  virtual const Objective * get(unsigned int n) const; 

  /**
   * Get a Objective from the ListOfObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Objective to get.
   * 
   * @return Objective in this ListOfObjectives
   * with the given @p sid or @c NULL if no such
   * Objective exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual Objective* get (const std::string& sid);


  /**
   * Get a Objective from the ListOfObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Objective to get.
   * 
   * @return Objective in this ListOfObjectives
   * with the given @p sid or @c NULL if no such
   * Objective exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const Objective* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfObjectives items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   * @return the item removed.  As mentioned above, the caller owns the
   * returned item.
   *
   * @see size()
   */
  virtual Objective* remove (unsigned int n);


  /**
   * Removes item in this ListOfObjectives items with the given identifier.
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
  virtual Objective* remove (const std::string& sid);


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., @link SBMLFbcTypeCode_t#SBML_FBC_OBJECTIVE SBML_FBC_OBJECTIVE@endlink).
   * 
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for objects contained in this list:
   * @link SBMLFbcTypeCode_t#SBML_FBC_OBJECTIVE SBML_FBC_OBJECTIVE@endlink
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;

  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ListOfObjective's "activeObjective" attribute has been set.
   *
   * @return @c true if this ListOfObjective's "activeObjective" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetActiveObjective() const;


  /**
   * Sets the value of the "activeObjective" attribute of this ListOfObjectives.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setActiveObjective(const std::string &activeObjective);


  /**
   * Returns the value of the "activeObjective" attribute of this ListOfObjectives.
   *
   * @return the value of the "activeObjective" attribute of this ListOfObjectives.
   */
  virtual const std::string &getActiveObjective() const;


  /**
   * Unsets the value of the "activeObjective" attribute of this ListOfObjectives.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetActiveObjective();

  /** @cond doxygenLibsbmlInternal */

  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond */
    
  /**
   * Adds a clone of all items in the provided ListOf to this object.  This means that when this ListOf is destroyed, the original items will not be destroyed.  In addition, copy over the input ListOfObjectives' 'activeObjective' attribute, if none is set for this element.
   *
   * @param list A list of items to be added.
   *
   * @see append(const SBase* item)
   */
  virtual int appendFrom(const ListOf* list);
  

  /**
   * Renames all the SIdRef attributes on this element if they match
   * @p oldid, but not any found in child or plugin elements.
   */
  virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);



protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * Create and return an SBML object of this class, if present.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */


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
   * Reads the attributes of corresponding package in SBMLDocument element.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the attributes of corresponding package in SBMLDocument element.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;  
  
  std::string mActiveObjective;
  /** @endcond */

};

/** @cond doxygenLibsbmlInternal */
/**
 * Used by ListOfObjectives::get() to lookup an SBase based by its 
 * symbol
 */
#ifndef SWIG
template<>
struct IdEq<Objective> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <Objective*> (sb)->getId() == id; }
};
#endif
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*
 * C API will be added here.
 */

LIBSBML_EXTERN
Objective_t *
Objective_create(unsigned int level, unsigned int version, unsigned int pkgversion);


LIBSBML_EXTERN
const char *
Objective_getId(Objective_t * obj);


LIBSBML_EXTERN
int
Objective_isSetId(Objective_t * obj);


LIBSBML_EXTERN
int
Objective_setId(Objective_t * obj, const char * id);


LIBSBML_EXTERN
int
Objective_unsetId(Objective_t * obj);

LIBSBML_EXTERN
const char *
Objective_getName(Objective_t * obj);


LIBSBML_EXTERN
int
Objective_isSetName(Objective_t * obj);


LIBSBML_EXTERN
int
Objective_setName(Objective_t * obj, const char * name);


LIBSBML_EXTERN
int
Objective_unsetName(Objective_t * obj);


LIBSBML_EXTERN
const char *
Objective_getType(Objective_t * obj);


LIBSBML_EXTERN
int
Objective_isSetType(Objective_t * obj);


LIBSBML_EXTERN
int
Objective_setType(Objective_t * obj, const char * type);


LIBSBML_EXTERN
int
Objective_unsetType(Objective_t * obj);



LIBSBML_EXTERN
int
Objective_addFluxObjective(Objective_t * obj, FluxObjective_t * flux);


LIBSBML_EXTERN
FluxObjective_t *
Objective_getFluxObjective(Objective_t * obj, unsigned int n);


LIBSBML_EXTERN
unsigned int
Objective_getNumFluxObjectives(Objective_t * obj);


LIBSBML_EXTERN
const char *
ListOfObjectives_getActiveObjective(ListOf_t * lo);


LIBSBML_EXTERN
int
ListOfObjectives_isSetActiveObjective(ListOf_t * lo);


LIBSBML_EXTERN
int
ListOfObjectives_setActiveObjective(ListOf_t * lo, const char * obj);


LIBSBML_EXTERN
int
ListOfObjectives_unsetActiveObjective(ListOf_t * lo);





LIBSBML_EXTERN
const char* 
ObjectiveType_toString(ObjectiveType_t type);


LIBSBML_EXTERN
ObjectiveType_t 
ObjectiveType_fromString(const char* s);


LIBSBML_EXTERN
int 
ObjectiveType_isValidObjectiveType(ObjectiveType_t type);


LIBSBML_EXTERN
int 
ObjectiveType_isValidObjectiveTypeString(const char* s);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Objective_H__ */
