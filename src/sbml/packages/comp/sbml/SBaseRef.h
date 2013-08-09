/**
 * @file    SBaseRef.h
 * @brief   Definition of SBaseRef, the SBase derived class of the comp package.
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
 * @class SBaseRef
 * @ingroup Comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %SBaseRef construct from the 'comp' package.
 *
 * The SBaseRef class was introduced by the SBML Level&nbsp;3
 * @ref Comp "Hierarchical Model Composition" package ('comp') as the
 * principle way by which submodel elements may be referenced.  The SBaseRef
 * class is usually found as the base class of a Port, Deletion, ReplacedElement,
 * or ReplacedBy class, but may appear as an child of one of the above
 * classes if the parent object references a Submodel.
 *
 * An SBaseRef object must reference an element using exactly one of the
 * optional attributes of the class.  Subclasses of SBaseRef may define
 * additional optional attributes that are legal ways to reference an element.
 *
 * SBaseRef objects may reference elements that do not live in the Model parent 
 * of the SBaseRef object.  However, the SBaseRef class itself does not 
 * provide a method of determining which Model or Submodel is being referenced.
 * The subclasses of SBaseRef provide methods for this instead.
 *
 * Once the Model to which the SBaseRef object is referencing has been established,
 * there are four optional attributes defined in the SBaseRef class that
 * are each methods of referencing an element:
 *
 * @li "portRef" (type PortSIdRef):  As its name implies, this attribute is used to
 * refer to a port identifier, in the case when the reference being
 * constructed with the SBaseRef is intended to refer to a port on a
 * submodel.  The namespace of the PortSIdRef value is the set
 * of identifiers of type PortSId defined in the submodel, not
 * the parent model.
 * @li "idRef" (type SIdRef): As its name implies, this attribute is used to
 * refer to a regular identifier (i.e., the value of an "id"
 * attribute on some other object), in the case when the reference being
 * constructed with the SBaseRef is intended to refer to an object that
 * does not have a port identifier.  The namespace of the SIdRef
 * value is the set of identifiers of type SId defined in the
 * submodel, not the parent model.
 * @li "unitRef" (type UnitSIdRef): This attribute is used to refer to the identifier
 * of a UnitDefinition object.  The namespace of the UnitSIdRef
 * value is the set of unit identifiers defined in the submodel, not the
 * parent model. (Note that even though this attribute is of type UnitSIdRef,
 * the reserved unit identifiers that are defined by SBML Level 3 (see
 * Section 3.1.10 of the core specification) are
 * *not* permitted as values of "unitRef".  Reserved unit
 * identifiers may not be replaced or deleted.)
 * @li "metaIdRef" (type IDREF): This attribute is used to refer to a "metaid"
 * attribute value on some other object, in the case when the reference
 * being constructed with the SBaseRef is intended to refer to an object
 * that does not have a port identifier.  The namespace of the "metaIdRef"
 * value is the entire document in which the referenced model resides, but
 * must refer to a subelement of the referenced model.  Since meta identifiers are
 * optional attributes of SBase, all SBML objects have the potential to
 * have a meta identifier value.
 *
 * An SBaseRef object may have up to one subcomponent named "sBaseRef", of
 * type SBaseRef.  This permits recursive structures to be constructed so
 * that objects inside submodels can be referenced.
 *
 * The form of such recursive references must be as follows.  The
 * highest-level SBaseRef object of such a chain (which will necessarily
 * be an object of class Port, Deletion, ReplacedElement or ReplacedBy,
 * because they are the only classes derived from the class SBaseRef) must
 * refer to a Submodel object in the containing model.  All child
 * SBaseRef objects in the chain must refer to components inside the
 * Model instance to which the Submodel refers.
 */


#ifndef SBaseRef_H__
#define SBaseRef_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/CompBase.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SBaseRef : public CompBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string   mMetaIdRef;
  std::string   mPortRef;
  std::string   mIdRef;
  std::string   mUnitRef;
  SBaseRef*     mSBaseRef;
  SBase*        mReferencedElement;
  SBase*        mDirectReference;
  /** @endcond */

public:

  /**
   * Creates a new SBaseRef with the given level, version, and package version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  SBaseRef(unsigned int level      = CompExtension::getDefaultLevel(),
           unsigned int version    = CompExtension::getDefaultVersion(),
           unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new SBaseRef with the given CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  SBaseRef(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   */
  SBaseRef(const SBaseRef& source);


  /**
   * Assignment operator.
   */
  SBaseRef& operator=(const SBaseRef& source);


  /**
   * Creates and returns a deep copy of this SBaseRef object.
   * 
   * @return a (deep) copy of this SBaseRef object
   */
  virtual SBaseRef* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~SBaseRef ();


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
   * Returns a List of all child SBase* objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all children objects.
   */
  virtual List* getAllElements(ElementFilter* filter=NULL);
  
  
  /**
   * Returns the value of the "metaIdRef" attribute of this SBaseRef.
   *
   * @return the value of the "metaIdRef" attribute of this SBaseRef.
   */
  virtual const std::string& getMetaIdRef () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SBaseRef's "metaIdRef" attribute has been set.
   *
   * @return @c true if this SBaseRef's "metaIdRef" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetMetaIdRef () const;

  
  /**
   * Sets the value of the "metaIdRef" attribute of this SBaseRef.
   *
   * This method fails if the id is not a valid syntax for an IDREF (@link
   * OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE
   * LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink), or if the SBaseRef already
   * points to an element of the submodel using a different interface (@link
   * OperationReturnValues_t#LIBSBML_OPERATION_FAILED
   * LIBSBML_OPERATION_FAILED @endlink).  An sBaseRef must use exactly one
   * method to point to a submodel element.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int setMetaIdRef (const std::string& id);


  /**
   * Unsets the value of the "metaIdRef" attribute of this SBaseRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetMetaIdRef ();


  /**
   * Returns the value of the "portRef" attribute of this SBaseRef.
   *
   * @return the value of the "portRef" attribute of this SBaseRef.
   */
  virtual const std::string& getPortRef () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SBaseRef's "portRef" attribute has been set.
   *
   * @return @c true if this SBaseRef's "portRef" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetPortRef () const;

  
  /**
   * Sets the value of the "portRef" attribute of this SBaseRef.  Fails if
   * the id is not a valid syntax for a PortSIdRef (@link
   * OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE
   * LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink), or if the SBaseRef already
   * points to an element of the submodel using a different interface (@link
   * OperationReturnValues_t#LIBSBML_OPERATION_FAILED
   * LIBSBML_OPERATION_FAILED @endlink).  An SBaseRef must use exactly one
   * method to point to a submodel element.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int setPortRef (const std::string& id);


  /**
   * Unsets the value of the "portRef" attribute of this SBaseRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetPortRef ();


  /**
   * Returns the value of the "idRef" attribute of this SBaseRef.
   *
   * @return the value of the "idRef" attribute of this SBaseRef.
   */
  virtual const std::string& getIdRef () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SBaseRef's "idRef" attribute has been set.
   *
   * @return @c true if this SBaseRef's "idRef" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetIdRef () const;

  
  /**
   * Sets the value of the "idRef" attribute of this SBaseRef.
   *
   * This method fails if the id is not a valid syntax for an SIdRef (@link
   * OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE
   * LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink), or if the SBaseRef already
   * points to an element of the submodel using a different interface (@link
   * OperationReturnValues_t#LIBSBML_OPERATION_FAILED
   * LIBSBML_OPERATION_FAILED @endlink).  A sBaseRef must use exactly one
   * method to point to a submodel element.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int setIdRef (const std::string& id);


  /**
   * Unsets the value of the "idRef" attribute of this SBaseRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetIdRef ();


  /**
   * Returns the value of the "unitRef" attribute of this SBaseRef.
   *
   * @return the value of the "unitRef" attribute of this SBaseRef.
   */
  virtual const std::string& getUnitRef () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SBaseRef's "unitRef" attribute has been set.
   *
   * @return @c true if this SBaseRef's "unitRef" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetUnitRef () const;

  
  /**
   * Sets the value of the "unitRef" attribute of this SBaseRef.
   *
   * This method fails if the id is not a valid syntax for a UnitSIdRef (@link
   * OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE
   * LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink), or if the SBaseRef already
   * points to an element of the submodel using a different interface (@link
   * OperationReturnValues_t#LIBSBML_OPERATION_FAILED
   * LIBSBML_OPERATION_FAILED @endlink).  A sBaseRef must use exactly one
   * method to point to a submodel element.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int setUnitRef (const std::string& id);


  /**
   * Unsets the value of the "unitRef" attribute of this SBaseRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetUnitRef ();


  /**
   * Get the child sBaseRef of this sBaseRef.
   * 
   * @return the const SBaseRef child of this SBaseRef, or NULL if none exists.
   */
  const SBaseRef* getSBaseRef () const;


  /**
   * Get the child sBaseRef of this SBaseRef.
   * 
   * @return the SBaseRef child of this SBaseRef, or NULL if none exists.
   */
  SBaseRef* getSBaseRef ();


  /**
   * Predicate for testing whether the sBaseRef for this SBaseRef is set.
   *
   * @return @c true if the sBaseRef of this SBaseRef is set, @c false
   * otherwise.
   */
  bool isSetSBaseRef () const;


  /**
   * Sets the sBaseRef definition of this SBaseRef to a copy of the given
   * SBaseRef object instance.
   *
   * This method fails if the added sBaseRef does not match the
   * level/version/package of the parent object or if the added sBaseRef cannot
   * be copied.
   *
   * @param sBaseRef the SBaseRef object instance to use.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * @li @link OperationReturnValues_t#LIBSBML_LEVEL_MISMATCH LIBSBML_LEVEL_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_VERSION_MISMATCH LIBSBML_VERSION_MISMATCH @endlink
   * @li @link OperationReturnValues_t#LIBSBML_PKG_VERSION_MISMATCH LIBSBML_VERSION_MISMATCH @endlink
   */
  int setSBaseRef (const SBaseRef* sBaseRef);


  /**
   * Creates a new, empty SBaseRef, adds it to this SBaseRef and 
   * returns the created SBaseRef.
   *
   * @return the newly created SBaseRef object instance.
   */
  SBaseRef* createSBaseRef ();


  /**
   * Unsets the child SBaseRef of this SBaseRef.  Deletes the former SBaseRef child,
   * if one existed.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetSBaseRef();


  /**
   * Returns how many elements are being referred to by this SBaseRef.  A
   * valid SBaseRef will have exactly one.  Possible referents are portRef,
   * idRef, unitRef, and metaIdRef.
   *
   * @return integer value between 0 and 4: the number of different ways this element points to its referent.
   */
  virtual int getNumReferents() const;


  /**
   * Returns true if getNumReferents() is exactly 1.
   *
   * @return boolean: 'true' if the attributes are correctly set; 'false' if not.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Renames all the SIdRef attributes on this element if they match
   * @p oldid, but not any found in child or plugin elements.
   */
  virtual void renameSIdRefs(std::string oldid, std::string newid);


  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


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


  /**
   * Examines the referenced Model for the referenced object, and returns it, if found.
   *
   * @param model the Model in which to look for the object referenced by
   * this SBaseRef.
   *
   * @return the element in the referenced Model to which this SBaseRef
   * refers.  If this object references an object in a Submodel, the returned
   * object will be in the instantiated Model in that Submodel.
   *
   */
  virtual SBase* getReferencedElementFrom(Model* model);


  /**
   * Finds and stores the referenced object by finding the Model it needs to
   * point to, calling 'saveReferencedElement' on its parent (which will also
   * be a SBaseRef or one of its subclasses), and the storing the result.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int saveReferencedElement();


  /**
   * Returns the object pointed to by this element.  If that element was
   * previously found and set with 'saveReferencedElement', that element is
   * returned; otherwise, 'saveReferencedElement' is called first, and the
   * found element is returned.
   */
  virtual SBase* getReferencedElement();


  /**
   * Removes the saved referenced element, if it had been saved earlier.
   */
  virtual void clearReferencedElement();


  /**
   * Deletes the referenced object (will delete the saved one, if it exists),
   * plus any other elements that element points to through ReplacedElement
   * or ReplacedBy children
   */
  virtual int performDeletion();


  /**
   * Finds this SBaseRef's parent, which can either be a List or can be
   * another SBaseRef, and tells it to remove this.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int removeFromParentAndDelete();


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
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "metaIdRef", mMetaIdRef );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Get the *direct* referenced object, which might be a Port.
   */
  virtual SBase* getDirectReference();
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  friend class CompModelPlugin; //for getDirectReference
  /** @endcond */
};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
SBaseRef_t *
SBaseRef_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion);


LIBSBML_EXTERN
void
SBaseRef_free(SBaseRef_t * sbr);


LIBSBML_EXTERN
SBaseRef_t *
SBaseRef_clone(SBaseRef_t * sbr);


LIBSBML_EXTERN
char *
SBaseRef_getPortRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
char *
SBaseRef_getIdRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
char *
SBaseRef_getUnitRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
char *
SBaseRef_getMetaIdRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
SBaseRef_t*
SBaseRef_getSBaseRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_isSetPortRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_isSetIdRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_isSetUnitRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_isSetMetaIdRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_isSetSBaseRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_setPortRef(SBaseRef_t * sbr, const char * portRef);


LIBSBML_EXTERN
int
SBaseRef_setIdRef(SBaseRef_t * sbr, const char * idRef);


LIBSBML_EXTERN
int
SBaseRef_setUnitRef(SBaseRef_t * sbr, const char * unitRef);


LIBSBML_EXTERN
int
SBaseRef_setMetaIdRef(SBaseRef_t * sbr, const char * metaIdRef);


LIBSBML_EXTERN
int
SBaseRef_setSBaseRef(SBaseRef_t * sbr, SBaseRef_t * sBaseRef);


LIBSBML_EXTERN
int
SBaseRef_unsetPortRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_unsetIdRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_unsetUnitRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_unsetMetaIdRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_unsetSBaseRef(SBaseRef_t * sbr);


LIBSBML_EXTERN
int
SBaseRef_hasRequiredAttributes(SBaseRef_t * sbr);



END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* SBaseRef_H__ */
