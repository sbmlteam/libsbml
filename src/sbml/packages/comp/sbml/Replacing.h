/**
 * @file    Replacing.h
 * @brief   Definition of Replacing, the SBaseRef-derived class of the comp package.
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
 *------------------------------------------------------------------------- -->
 *
 * @class Replacing
 * @ingroup Comp
 * @brief @htmlinclude pkg-marker-comp.html
 * A convenience subclass of the %ReplacedElement and %ReplacedBy 
 * constructs from the 'comp' package.
 *
 * The Replacing class does not exist officialy in the the
 * @ref Comp "Hierarchical Model Composition" package ('comp'),
 * but is implemented here as a convenience subclass of the
 * ReplacedElement and ReplacedBy classes, since both of those classes 
 * define a 'submodelRef' attribute.
 *
 * The required attribute "submodelRef" takes a value of type
 * SIdRef, which must be the identifier of a Submodel object in
 * the containing model.  The model referenced by the
 * Submodel object establishes the object namespaces for the
 * "portRef", "idRef", "unitRef" and "metaIdRef"
 * attributes: only objects within the Model object may be referenced by
 * those attributes.
 */


#ifndef Replacing_H__
#define Replacing_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/SBaseRef.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Replacing : public SBaseRef
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string   mSubmodelRef;
  std::string   mConversionFactor;
  /** @endcond */

public:

  /**
   * Creates a new Replacing with the given level, version, and package
   * version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  Replacing(unsigned int level      = CompExtension::getDefaultLevel(),
            unsigned int version    = CompExtension::getDefaultVersion(),
            unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new Replacing with the given CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  Replacing(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   */
  Replacing(const Replacing& source);


  /**
   * Assignment operator.
   */
  Replacing& operator=(const Replacing& source);


  /**
   * Destructor.
   */ 
  virtual ~Replacing ();


  /**
   * Returns the value of the "submodelRef" attribute of this SBaseRef.
   *
   * @return the value of the "submodelRef" attribute of this SBaseRef.
   */
  virtual const std::string& getSubmodelRef () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SBaseRef's "submodelRef" attribute has been set.
   *
   * @return @c true if this SBaseRef's "submodelRef" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSubmodelRef () const;

  
  /**
   * Sets the value of the "submodelRef" attribute of this SBaseRef.  Fails
   * if the id is not a valid syntax for an SIdRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setSubmodelRef (const std::string& id);


  /**
   * Unsets the value of the "SubmodelRef" attribute of this SBaseRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetSubmodelRef ();


  /**
   * Returns true if getNumReferents() is exactly 1 and if the submodelRef is set.
   *
   * @return boolean: 'true' if the attributes are correctly set; 'false' if not.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Finds and stores the referenced object.  Finds the Submodel to which
   * it refers, getting the instantiated Model inside that Submodel, calling
   * 'getReferencedElementFrom' on that model, and storing the result.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int saveReferencedElement();


  /**
   * Renames all the SIdRef attributes on this element if they match
   * @p oldid, but not any found in child or plugin elements.
   */
  virtual void renameSIdRefs(std::string oldid, std::string newid);


  /**
   * Removes the redundant element from instantiated submodels, and points
   * all old references to the remaining element (different for
   * ReplacedElements and ReplacedBy elements.
   */
  virtual int performReplacement() = 0;


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
  /* Replaces the referenced object with the replacement element, instead of the direct parent of this replaced element.  This is so we can call this recursively, when replacing something that itself replaces something.
   */
  virtual int replaceWithAndMaybeDelete(SBase* replacement, bool deleteme, ASTNode* conversionFactor);
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


  /** @cond doxygenLibsbmlInternal */
  /**
   * Searches the model that @p oldnames came from for references to any of its ids,
   * and replaces them with references to @p newnames.  
   *
   * @param oldnames the object being replaced, and whose parent Model contains
   * the references that need to be updated.
   *
   * @param newnames the object that should now be referenced instead, to which 
   * any references to @p oldnames should now point.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   */
  virtual int updateIDs(SBase* oldnames, SBase* newnames);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Modify mathematical references to the referenced object according to the @p conversionFactor.  
   *
   * Find all idrefs in the Model that have mathematical meaning, and convert 
   * them to instead reference the @p replacement, modified according to the 
   * @p conversionFactor.  Will modify the referenced object's use in MathML 
   * nodes according to replacementID/conversionFactor, and will multiply the 
   * MathML of elements that assign to the referenced object by the @p conversionFactor.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int performConversions(SBase* replacement, ASTNode*& conversionFactor);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Multiply this element's conversion factor (if present) to the @p conversionFactor.
   */
  virtual int convertConversionFactor(ASTNode*& conversionFactor);
  /** @endcond */
};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Replacing_H__ */
