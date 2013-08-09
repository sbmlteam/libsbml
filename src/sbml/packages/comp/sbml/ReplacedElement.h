/**
 * @file    ReplacedElement.h
 * @brief   Definition of ReplacedElement, the SBase derived class of replacedElements package.
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
 * @class ReplacedElement
 * @ingroup Comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %ReplacedElement construct from the 'comp' package.
 *
 * The ReplacedElement class was introduced by the SBML Level&nbsp;3
 * @ref Comp "Hierarchical Model Composition" package ('comp')
 * to allow submodel elements
 * to be replaced, but still allow references to those elements to be valid.
 * A ReplacedElement object is essentially a pointer to a submodel object
 * that should be considered 'replaced'.  The object holding the
 * ReplacedElement instance is the one doing the replacing; the object
 * pointed to by the ReplacedElement object is the object being replaced.
 *
 * A replacement implies that dependencies involving the replaced object
 * must be updated: all references to the replaced object elsewhere in the
 * model are taken to refer to the replacement object instead.  For
 * example, if one species replaces another, then any reference to the
 * original species in mathematical formulas, or lists of reactants or
 * products or modifiers in reactions, or initial assignments, or any other
 * SBML construct, are taken to refer to the replacement species, with its
 * value possibly modified by either this object's "conversionFactor"
 * attribute or the relevant submodel's conversion factors. Moreover, any 
 * annotations that refer to the
 * replaced species' "metaid" value must be made to refer to the
 * replacement species' "metaid" value instead; and anything else
 * that referred either to an object identifier (i.e., attributes such as
 * the "id" attribute whose types inherit from the SId
 * primitive data type) or the meta identifier (i.e., the "metaid"
 * attribute or any other attribute that inherits from the ID primitive
 * data type) must be made to refer to the replacement species object
 * instead.
 *
 * It is worth noting that local parameters (inside Reaction objects) pose an
 * interesting edge case for these rules. In order to determine which element
 * is pointed to by a <code>&lt;cn&gt;</code> element within the
 * <code>&lt;math&gt;</code> element of a KineticLaw object, it is necessary
 * to examine the local parameters of that kinetic law's parent Reaction
 * object.  Whether the <code>&lt;cn&gt;</code> element is considered to
 * point to something new, then, depends on whether it pointed to the local
 * parameter and whether that local parameter was replaced, even if the text
 * of the element matched the SId value of another element in the model.
 * Note that local parameters may only effectively be replaced by global
 * parameters, since references to its SId are only valid from within the
 * Reaction element to which it belongs.
 *
 * When referencing an element within the Submodel pointed to by the 
 * "submodelRef" attribute (defined in libSBML in the Replacing class), 
 * any of the four attributes inherited from 
 * SBaseRef for the purpose may be used (portRef, idRef, unitRef, or 
 * metaIdRef), or a new optional attribute "deletion" may be used.  This
 * attribute must be the identifier of a Deletion
 * object in the parent Model of the ReplacedElement (i.e., the value of
 * some Deletion object's "id" attribute).  When "deletion" is
 * set, it means the ReplacedElement object is actually an annotation to
 * indicate that the replacement object replaces something deleted
 * from a submodel.  The use of the "deletion" attribute overrides
 * the use of the attributes inherited from SBaseRef: instead of using,
 * e.g., "portRef" or "idRef", the ReplacedElement instance
 * sets "deletion" to the identifier of the Deletion object.  In
 * addition, the referenced Deletion must be a child of the Submodel
 * referenced by the "submodelRef" attribute.
 *
 * The use of ReplacedElement objects to refer to deletions has no effect
 * on the composition of models or the mathematical properties of the
 * result.  It serves instead to help record the decision-making process
 * that lead to a given model.  It can be particularly useful for
 * visualization purposes, as well as to serve as scaffolding where other
 * types of annotations can be added using the normal Annotation
 * subcomponents available on all SBase objects in SBML.
 *
 * As with the Submodel class, it may be that the units of the replaced
 * element may not match the units of the replacement element.  In this case, 
 * the optional "conversionFactor" attribute may be used.  This attribute, if
 * present, defines how to transform or rescale the replaced object's value
 * so that it is appropriate for the new contexts in which the object
 * appears.  This attribute takes a value of type SIdRef, and
 * the value must refer to a Parameter object instance defined in the
 * model.  This parameter then acts as a conversion factor.
 * 
 * The value of the conversion factor should be defined such that a single
 * unit of the replaced element multiplied by the conversion factor should
 * equal a single unit of the replacement element, and the units of the
 * conversion factor should be commensurate with that transformation.  The
 * referenced Parameter may be non-constant, particularly if a Species is
 * replaced by a Species with a different "hasOnlySubstanceUnits"
 * attribute value, thus changing amount to concentration, or visa versa.
 */


#ifndef ReplacedElement_H__
#define ReplacedElement_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/Replacing.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ReplacedElement : public Replacing
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string   mDeletion;
  /** @endcond */

public:

  /**
   * Creates a new ReplacedElement with the given level, version, and package
   * version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  ReplacedElement(unsigned int level      = CompExtension::getDefaultLevel(),
                  unsigned int version    = CompExtension::getDefaultVersion(),
                  unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new ReplacedElement with the given CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  ReplacedElement(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   */
  ReplacedElement(const ReplacedElement& source);


  /**
   * Assignment operator.
   */
  ReplacedElement& operator=(const ReplacedElement& source);


  /**
   * Creates and returns a deep copy of this ReplacedElement object.
   * 
   * @return a (deep) copy of this ReplacedElement object
   */
  virtual ReplacedElement* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~ReplacedElement ();


  /**
   * Returns the value of the "conversionFactor" attribute of this ReplacedElement.
   *
   * @return the value of the "conversionFactor" attribute of this ReplacedElement.
   */
  virtual const std::string& getConversionFactor () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ReplacedElement's "conversionFactor" attribute has been set.
   *
   * @return @c true if this ReplacedElement's "conversionFactor" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetConversionFactor () const;

  
  /**
   * Sets the value of the "conversionFactor" attribute of this ReplacedElement.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setConversionFactor (const std::string& id);


  /**
   * Unsets the value of the "conversionFactor" attribute of this ReplacedElement.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetConversionFactor ();


  /**
   * Returns the value of the "deletion" attribute of this ReplacedElement.
   *
   * @return the value of the "deletion" attribute of this ReplacedElement.
   */
  virtual const std::string& getDeletion () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SBaseRef's "deletion" attribute has been set.
   *
   * @return @c true if this ReplacedElement's "deletion" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDeletion () const;

  
  /**
   * Sets the value of the "deletion" attribute of this ReplacedElement.
   *
   * This method fails if the id is not a valid syntax for an SIdRef (@link
   * OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE
   * LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink), or if the SBaseRef already
   * points to an element of the submodel using a different interface (@link
   * OperationReturnValues_t#LIBSBML_OPERATION_FAILED
   * LIBSBML_OPERATION_FAILED @endlink).  A ReplacedElement must use exactly
   * one method to point to a submodel element: deletion, port, idRef,
   * unitRef, or metaIdRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int setDeletion (const std::string& id);


  /**
   * Unsets the value of the "deletion" attribute of this ReplacedElement.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetDeletion ();


  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * Returns how many elements are being referred to by this ReplacedElement.  A
   * valid ReplacedElement will have exactly one.  Possible referents are deletion,
   * port, idRef, unitRef, and metaIdRef.
   *
   * @return integer value between 0 and 5: the number of different ways this
   * element points to its referent.
   */
  virtual int getNumReferents() const;


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


  /**
   * Renames all the SIdRef attributes on this element if they match
   * @p oldid, but not any found in child or plugin elements.
   */
  virtual void renameSIdRefs(std::string oldid, std::string newid);


  /**
   * Removes the referenced element from instantiated submodels, and points
   * all old references to that element to the replacement element.
   */
  virtual int performReplacement();


  /**
   * Finds the SBase object this ReplacedElement object points to, if any.
   *
   */
  virtual SBase* getReferencedElementFrom(Model* model);


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
   *   stream.writeAttribute( "conversionFactor", mConversionFactor );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */

};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
ReplacedElement_t *
ReplacedElement_create(unsigned int level, unsigned int version,
                       unsigned int pkgVersion);


LIBSBML_EXTERN
void
ReplacedElement_free(ReplacedElement_t * re);


LIBSBML_EXTERN
ReplacedElement_t *
ReplacedElement_clone(ReplacedElement_t * re);


LIBSBML_EXTERN
char *
ReplacedElement_getSubmodelRef(ReplacedElement_t * re);


LIBSBML_EXTERN
char *
ReplacedElement_getDeletion(ReplacedElement_t * re);


LIBSBML_EXTERN
char *
ReplacedElement_getConversionFactor(ReplacedElement_t * re);


LIBSBML_EXTERN
int
ReplacedElement_isSetSubmodelRef(ReplacedElement_t * re);


LIBSBML_EXTERN
int
ReplacedElement_isSetDeletion(ReplacedElement_t * re);


LIBSBML_EXTERN
int
ReplacedElement_isSetConversionFactor(ReplacedElement_t * re);


LIBSBML_EXTERN
int
ReplacedElement_setSubmodelRef(ReplacedElement_t * re, const char * submodelRef);


LIBSBML_EXTERN
int
ReplacedElement_setDeletion(ReplacedElement_t * re, const char * deletion);


LIBSBML_EXTERN
int
ReplacedElement_setConversionFactor(ReplacedElement_t * re, const char * conversionFactor);


LIBSBML_EXTERN
int
ReplacedElement_unsetSubmodelRef(ReplacedElement_t * re);


LIBSBML_EXTERN
int
ReplacedElement_unsetDeletion(ReplacedElement_t * re);


LIBSBML_EXTERN
int
ReplacedElement_unsetConversionFactor(ReplacedElement_t * re);


LIBSBML_EXTERN
int
ReplacedElement_hasRequiredAttributes(ReplacedElement_t * re);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ReplacedElement_H__ */
