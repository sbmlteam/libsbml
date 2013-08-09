/**
 * @file    ExternalModelDefinition.h
 * @brief   Definition of ExternalModelDefinition, the SBase derived class of the comp package.
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
 * @class ExternalModelDefinition
 * @ingroup Comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %ExternalModelDefinition construct from the 'comp' package.
 *
 * The ExternalModelDefinition class was introduced by the SBML Level&nbsp;3
 * @ref Comp "Hierarchical Model Composition" package ('comp')
 * to define references to
 * Model objects defined in other files.
 * 
 * ExternalModelDefinition objects are model definitions&mdash;in and of
 * themselves, they are definitions of models but not uses of those models.
 * The class provides a way to declare and identify them so that Model
 * objects in the present SBML document can use them in Submodel objects.
 * 
 * ExternalModelDefinition contains two required attributes
 * ("source" and "id") and three optional attributes
 * ("modelRef", "md5" and "name").
 *  
 * The "id" attribute serves to provide a handle for the external
 * model reference so that Submodel objects can refer to it.  Crucially,
 * it is not the identifier of the model being referenced; rather,
 * it is an identifier for this ExternalModelDefinition object within the
 * current SBML document.  The "id" attribute takes a required value
 * of type SId, and must be unique across all Model and ExternalModelDefinition
 * objects present in the document.
 * 
 * ExternalModelDefinition also has an optional "name" attribute, of
 * type 'string'.  The "name" attribute may be used to provide
 * a human-readable description of the ExternalModelDefintion object.
 * 
 * The required attribute "source" is used to locate the SBML document
 * containing an external model definition.  The value of this attribute must
 * be of type anyURI.  Since URIs may be either URLs, URNs, or relative or
 * absolute file locations, this offers flexibility in referencing SBML
 * documents.  In all cases, the "source" attribute value must refer
 * specifically to an SBML Level&nbsp;3 Version&nbsp;1 document; prior
 * Levels/Versions of SBML are not supported by this package.  The entire
 * file at the given location is referenced.  The "source" attribute must
 * have a value for every ExternalModelDefinition instance.
 * 
 * ExternalModelDefinition's optional attribute "modelRef", of type
 * SIdRef, is used to identify a Model or
 * ExternalModelDefinition object within the SBML document located at
 * "source".  The object referenced may be the main model in the
 * document, or it may be a model definition contained in the SBML
 * document's ListOfModelDefinitions or
 * ListOfExternalModelDefinitions lists.  Loops are not allowed: it
 * must be possible to follow a chain of ExternalModelDefinition objects
 * to its end in a Model object.
 * 
 * In core SBML, the "id" on Model is an optional attribute, and therefore,
 * it is possible that the Model object in a given SBML document does not
 * have an identifier.  In that case, there is no value to give to the
 * "modelRef" attribute in ExternalModelDefinition.  If "modelRef" does not
 * have a value, then the main model (i.e., the <code>&lt;model&gt;</code>
 * element within the <code>&lt;sbml&gt;</code> element) in the referenced
 * file is interpreted as being the model referenced by this
 * ExternalModelDefinition instance.
 * 
 * Finally, the optional "md5" attribute takes a string value.  If
 * set, it must be an MD5 checksum value computed over the document
 * referenced by "source".  This checksum can serve as a data
 * integrity check over the contents of the "source".  Applications
 * may use this to verify that the contents have not changed since the time
 * that the ExternalModelDefinition reference was constructed.
 */

#ifndef ExternalModelDefinition_H__
#define ExternalModelDefinition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/CompBase.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ExternalModelDefinition : public CompBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string   mId;
  std::string   mName;
  std::string   mSource;
  std::string   mModelRef;
  std::string   mMd5;
  /** @endcond */

public:

  /**
   * Creates a new ExternalModelDefinition with the given level, version, and
   * package version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  ExternalModelDefinition(unsigned int level      = CompExtension::getDefaultLevel(),
                          unsigned int version    = CompExtension::getDefaultVersion(),
                          unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new ExternalModelDefinition with the given CompPkgNamespaces
   * object.
   *
   * @param compns the namespace to use.
   */
  ExternalModelDefinition(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   *
   * @param source the object to copy.
   */
  ExternalModelDefinition(const ExternalModelDefinition& source);


  /**
   * Assignment operator.
   */
  ExternalModelDefinition& operator=(const ExternalModelDefinition& source);


 /**
   * Creates and returns a deep copy of this ExternalModelDefinition object.
   * 
   * @return a (deep) copy of this ExternalModelDefinition object
   */
  virtual ExternalModelDefinition* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~ExternalModelDefinition ();


  /**
   * Sets the value of the "id" attribute of this ExternalModelDefinition.
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
  virtual int setId (const std::string &id);


  /**
   * Returns the value of the "id" attribute of this ExternalModelDefinition.
   * 
   * @return the name of this ExternalModelDefinition.
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
   * Unsets the value of the "id" attribute of this ExternalModelDefinition.
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
   * Sets the value of the "name" attribute of this ExternalModelDefinition.
   *
   * The string in @p name is copied.
   *
   * @param name the new name for the ExternalModelDefinition
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
   * Returns the value of the "name" attribute of this
   * ExternalModelDefinition.
   * 
   * @return the name of this ExternalModelDefinition.
   */
  virtual const std::string& getName () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * object's "name" attribute has been set.
   *
   * @htmlinclude comment-set-methods.html
   * 
   * @return @c true if the "name" attribute of this object has been
   * set, @c false otherwise.
   */
  virtual bool isSetName() const;


  /**
   * Unsets the value of the "name" attribute of this
   * ExternalModelDefinition.
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
   * Returns the value of the "modelRef" attribute of this
   * ExternalModelDefinition.
   *
   * @return the value of the "modelRef" attribute of this
   * ExternalModelDefinition.
   */
  virtual const std::string& getModelRef () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ExternalModelDefinition's "modelRef" attribute has been set.
   *
   * @return @c true if this ExternalModelDefinition's "modelRef" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetModelRef () const;

  
  /**
   * Sets the value of the "modelRef" attribute of this
   * ExternalModelDefinition.  Fails if the @p id is not a valid syntax for an
   * SIdRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setModelRef (const std::string& id);


  /**
   * Unsets the value of the "modelRef" attribute of this
   * ExternalModelDefinition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetModelRef ();


  /**
   * Returns the value of the "md5" attribute of this
   * ExternalModelDefinition.
   *
   * @return the value of the "md5" attribute of this
   * ExternalModelDefinition.
   */
  virtual const std::string& getMd5 () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ExternalModelDefinition's "md5" attribute has been set.
   *
   * @return @c true if this ExternalModelDefinition's "md5" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetMd5 () const;

  
  /**
   * Sets the value of the "md5" attribute of this ExternalModelDefinition.
   * 
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setMd5 (const std::string& md5);


  /**
   * Unsets the value of the "md5" attribute of this ExternalModelDefinition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetMd5 ();


  /**
   * Returns the value of the "source" attribute of this
   * ExternalModelDefinition.
   *
   * @return the value of the "source" attribute of this
   * ExternalModelDefinition.
   */
  virtual const std::string& getSource () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ExternalModelDefinition's "source" attribute has been set.
   *
   * @return @c true if this ExternalModelDefinition's "source" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetSource () const;

  
  /**
   * Sets the value of the "source" attribute of this
   * ExternalModelDefinition.
   *
   * @param source the value to use for the "source" attribute.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setSource (const std::string& source);


  /**
   * Unsets the value of the "source" attribute of this
   * ExternalModelDefinition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetSource ();


  /**
   * Returns true if the "modelRef" and "id" attributes are set, and false if not.
   *
   * This method does not check to see if the referred-to model actually
   * exists.
   *
   * @return boolean: @c true if the attributes are correctly set; @c false
   * if not.
   */
  virtual bool hasRequiredAttributes() const;


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
   * <pre>
   *   SBase::writeElements(stream);
   *   mReactants.write(stream);
   *   mProducts.write(stream);
   *   ...
   * <pre>
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

  /**
   * Resolves and returns the referenced Model object of this ExternalModelDefinition.
   * If none can be found, an error is set and NULL is returned.  The
   * returned Model is a non-owning pointer to the model; the original
   * Model is saved (along with the SBMLDocument from which it comes) as
   * a child of the CompSBMLDocumentPlugin of the SBMLDocument to which this
   * Model belongs.  If this ExternalModelDefinition is not part of any
   * SBMLDocument, NULL will be returned.
   */
  virtual Model* getReferencedModel();


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
   *   stream.writeAttribute( "submodel" , mSubmodel );
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
ExternalModelDefinition_t *
ExternalModelDefinition_create(unsigned int level, unsigned int version,
                               unsigned int pkgVersion);


LIBSBML_EXTERN
void
ExternalModelDefinition_free(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
ExternalModelDefinition_t *
ExternalModelDefinition_clone(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
char *
ExternalModelDefinition_getId(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
char *
ExternalModelDefinition_getSource(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
char *
ExternalModelDefinition_getName(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
char *
ExternalModelDefinition_getModelRef(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_isSetId(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_isSetSource(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_isSetName(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_isSetModelRef(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_setId(ExternalModelDefinition_t * emd, const char * id);


LIBSBML_EXTERN
int
ExternalModelDefinition_setSource(ExternalModelDefinition_t * emd, const char * source);


LIBSBML_EXTERN
int
ExternalModelDefinition_setName(ExternalModelDefinition_t * emd, const char * name);


LIBSBML_EXTERN
int
ExternalModelDefinition_setModelRef(ExternalModelDefinition_t * emd, const char * modelRef);


LIBSBML_EXTERN
int
ExternalModelDefinition_unsetId(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_unsetSource(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_unsetName(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_unsetModelRef(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
int
ExternalModelDefinition_hasRequiredAttributes(ExternalModelDefinition_t * emd);


LIBSBML_EXTERN
ExternalModelDefinition_t *
ListOfExternalModelDefinitions_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
ExternalModelDefinition_t *
ListOfExternalModelDefinitions_removeById(ListOf_t * lo, const char * sid);



END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ExternalModelDefinition_H__ */
