/**
 * @file    Deletion.h
 * @brief   Definition of Deletion, the SBase derived class of deletions package.
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
 * @class Deletion
 * @ingroup comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %Deletion construct from the 'comp' package.
 *
 * The Deletion class was introduced by the SBML Level&nbsp;3
 * @ref comp "Hierarchical Model Composition" package ('comp')
 * to allow elements of submodels to be
 * removed before instantiation.

 * The Deletion object class is used to define a deletion operation
 * to be applied when a submodel instantiates a model definition.
 * Deletions may be useful in hierarchical model composition scenarios for
 * various reasons.  For example, some components in a submodel may be
 * redundant in the composed model, perhaps because the same features are
 * implemented in a different way in the new model.

 * Deletions function as follows.  When the Model to which the Submodel
 * object refers (via the "modelRef" attribute) is read and processed for
 * inclusion into the composed model, each Deletion object identifies an
 * object to remove from that Model instance.  The resulting submodel
 * instance consists of everything in the Model object instance minus the
 * entities referenced by the list of Deletion objects.
 * 
 * As might be expected, deletions can have wide-ranging implications,
 * especially when the object deleted has substantial substructure, as in
 * the case of reactions.  The following are rules regarding deletions and
 * their effects.
 * 
 * @li An object that has been deleted is considered inaccessible.
 * Any element that has been deleted (or replaced)
 * may not be referenced by an SBaseRef object.
 * @li If the deleted object has child objects and other structures, the
 * child objects and substructure are also considered to be deleted.
 * @li It is not an error to delete explicitly an object that is already
 * deleted by implication (for example as a result of the second point
 * above).  The resulting model is the same.
 * @li If the deleted object is from an SBML namespace that is not
 * understood by the interpreter, the deletion must be ignored&mdash;the 
 * object will not need to be deleted, as the interpreter could not
 * understand the package.  If an interpreter cannot tell whether 
 * a referenced object does not exist or if exists in an unparsed namespace
 * it may produce a warning.
 *
 * The Deletion object class is subclassed from SBaseRef, and reuses all the 
 * machinery provided by SBaseRef.  In addition, it defines two optional 
 * attributes, "id" and "name".  The "id" attribute can be used to give an
 * identifier to a given deletion operation.  The identifier has no
 * mathematical meaning, but it may be useful for creating submodels that
 * can be manipulated more directly by other submodels.  (Indeed, it is
 * legitimate for an enclosing model definition to delete a deletion!)
 *
 * The optional "name" attribute is provided on Deletion for the
 * same reason it is provided on other elements that have identifiers;
 * viz., to provide for the possibility of giving a human-readable name to
 * the object.  The name may be useful in situations when deletions are
 * displayed to modelers.
 */


#ifndef Deletion_H__
#define Deletion_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/packages/comp/sbml/SBaseRef.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Deletion : public SBaseRef
{
protected:

  /** @cond doxygenLibsbmlInternal */
  std::string mId;
  std::string mName;
  /** @endcond */

public:

  /**
   * Creates a new Deletion with the given level, version, and package version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  Deletion(unsigned int level      = CompExtension::getDefaultLevel(),
           unsigned int version    = CompExtension::getDefaultVersion(),
           unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new Deletion with the given CompPkgNamespaces object.
   */
  Deletion(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   */
  Deletion(const Deletion& source);


  /**
   * Assignment operator.
   */
  Deletion& operator=(const Deletion& source);


  /**
   * Creates and returns a deep copy of this Deletion object.
   * 
   * @return a (deep) copy of this Deletion object
   */
  virtual Deletion* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~Deletion ();


  /**
   * Sets the value of the "id" attribute of this Deletion.
   *
   * This method fails if the @p id is not a valid syntax for an SId.
   *
   * @param id the identifier to use
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setId (const std::string& id);


  /**
   * Returns the value of the "id" attribute of this Deletion.
   * 
   * @return the name of this Deletion.
   */
  virtual const std::string& getId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * object's "id" attribute has been set.
   *
   * @htmlinclude comment-set-methods.html
   * 
   * @return @c true if the "id" attribute of this object has been
   * set, @c false otherwise.
   */
  virtual bool isSetId() const;


  /**
   * Unsets the value of the "id" attribute of this Deletion.
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
   * Sets the value of the "name" attribute of this Deletion.
   *
   * The string in @p name is copied.
   *
   * @param name the new name for the Deletion
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setName (const std::string& name);


  /**
   * Returns the value of the "name" attribute of this Deletion.
   * 
   * @return the name of this Deletion.
   */
  virtual const std::string& getName () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * object's "name" attribute has been set.
   *
   * @htmlinclude comment-set-methods.html
   * 
   * @return @c true if the "name" attribute of this object has been set, @c
   * false otherwise.
   */
  virtual bool isSetName() const;


  /**
   * Unsets the value of the "name" attribute of this Deletion.
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
   * Returns the XML element name of this SBML object.
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
   * @return the typecode (an integer) of this SBML object or @c SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


  /**
   * Finds and stores the referenced object.  It finds its Submodel parent,
   * gets its instantiated Model object, calls
   * 'getReferencedElementFrom' on that model, and stores the result.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int saveReferencedElement();
  
  
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
   *   stream.writeAttribute( "id"  , mId   );
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
Deletion_t *
Deletion_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion);


LIBSBML_EXTERN
void
Deletion_free(Deletion_t * d);


LIBSBML_EXTERN
Deletion_t *
Deletion_clone(Deletion_t * d);


LIBSBML_EXTERN
char *
Deletion_getId(Deletion_t * d);


LIBSBML_EXTERN
char *
Deletion_getName(Deletion_t * d);


LIBSBML_EXTERN
int
Deletion_isSetId(Deletion_t * d);


LIBSBML_EXTERN
int
Deletion_isSetName(Deletion_t * d);


LIBSBML_EXTERN
int
Deletion_setId(Deletion_t * d, const char * id);


LIBSBML_EXTERN
int
Deletion_setName(Deletion_t * d, const char * name);


LIBSBML_EXTERN
int
Deletion_unsetId(Deletion_t * d);


LIBSBML_EXTERN
int
Deletion_unsetName(Deletion_t * d);


LIBSBML_EXTERN
int
Deletion_hasRequiredAttributes(Deletion_t * d);


LIBSBML_EXTERN
Deletion_t *
ListOfDeletions_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
Deletion_t *
ListOfDeletions_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Deletion_H__ */
