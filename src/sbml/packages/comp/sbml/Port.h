/**
 * @file    Port.h
 * @brief   Definition of Port, the SBase derived class of the comp package.
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
 * @class Port
 * @ingroup comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %Port construct from the &ldquo;comp&rdquo; package.
 *
 * The Port class was introduced by the SBML Level&nbsp;3
 * @ref comp "Hierarchical Model Composition" package (&ldquo;comp&rdquo;)
 * to allow a Model to define a standard
 * interface between it and other models that might use it as a submodel.  It
 * derives from the SBaseRef class, and the elements defined there refer to
 * elements in the same parent Model as the Port object.  A Port object
 * instance therefore uses those attributes to define a port for a component
 * in a model.  When other SBaseRef or SBaseRef-derived classes refer to a
 * Port object using a "portRef" attribute, the element being referenced is
 * the element the Port object itself points to.
 *
 * In the present formulation of the Hierarchical %Model Composition
 * package, the use of ports is not enforced, nor is there any
 * mechanism to restrict which ports may be used in what ways&mdash;they are
 * only an advisory construct.  Future versions of this SBML package may
 * provide additional functionality to support explicit restrictions on
 * port use.  For the present definition of Hierarchical %Model Composition,
 * users of models containing ports are encouraged to respect the modeler's
 * intention in defining ports, and use the port definitions to interact
 * with components through their ports (when they have ports defined)
 * rather than interact directly with the components.
 *
 * The required attribute "id" is used to give an identifier to a
 * Port object so that other objects can refer to it.  The attribute has
 * type PortSId and is essentially identical to the SBML
 * primitive type SId, except that its namespace is limited to
 * the identifiers of Port objects defined within a Model object.  In
 * parallel, the PortSId type has a companion type,
 * PortSIdRef, that corresponds to the SBML primitive type
 * SIdRef; the value space of PortSIdRef is limited
 * to PortSId values.  
 */


#ifndef Port_H__
#define Port_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/SBaseRef.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Port : public SBaseRef
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string   mId;
  std::string   mName;
  /** @endcond */

public:

  /**
   * Creates a new Port with the given level, version, and package version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  Port(unsigned int level      = CompExtension::getDefaultLevel(),
       unsigned int version    = CompExtension::getDefaultVersion(),
       unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new Port with the given CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  Port(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   */
  Port(const Port& source);


  /**
   * Assignment operator.
   */
  Port& operator=(const Port& source);


  /**
   * Creates and returns a deep copy of this Port object.
   * 
   * @return a (deep) copy of this Port object
   */
  virtual Port* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~Port ();


  /**
   * Returns the value of the "id" attribute of this Port.
   *
   * @return the value of the "id" attribute of this Port.
   */
  virtual const std::string& getId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Port's "id" attribute has been set.
   *
   * @return @c true if this Port's "id" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetId () const;

  
  /**
   * Sets the value of the "id" attribute of this Port.
   *
   * This method fails if the @p id is not a valid syntax for an SId.
   *
   * @param id the identifier for the port
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
   * Unsets the value of the "id" attribute of this Port.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetId ();


  /**
   * Returns the value of the "name" attribute of this Port.
   *
   * @return the value of the "name" attribute of this Port.
   */
  virtual const std::string& getName () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Port's "name" attribute has been set.
   *
   * @return @c true if this Port's "name" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetName () const;

  
  /**
   * Sets the value of the "name" attribute of this Port.
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
   * Unsets the value of the "name" attribute of this Port.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetName ();


  /**
   * Overrides SBaseRef::setPortRef to always fail, because Port objects
   * themselves cannot refer to model elements by PortSId.
   *
   * @param id the identifier to set for the port reference
   *
   * @return integer value indicating failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible value
   * returned by this function is:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int setPortRef (const std::string& id);


  /**
   * Returns true if the 'id' attribute is set, and if exactly one of
   * the optional attributes of SBaseRef (portRef, idRef, metaIdRef, 
   * and unitRef)are set.
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
   * Returns the libSBML type code for this SBML object.
   * 
   * LibSBML attaches an identifying code to every kind of SBML object.
   * These are known as <em>SBML type codes</em>.  @if clike The set of
   * possible type codes for the &ldquo;comp&rdquo; package is defined in the enumeration
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
   * Finds and stores the referenced object by finding its Model parent,
   * calling 'getReferencedElementFrom()' on that model, and storing the
   * result.
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
   * Renames the idRef attribute on this element if the oldid matches.
   */
  virtual void renameSIdRefs(std::string oldid, std::string newid);


  /**
   * Renames the unitRef attribute on this element if the oldid matches.
   */
  virtual void renameUnitSIdRefs(std::string oldid, std::string newid);


  /**
   * Renames the metaIdRef attribute on this element if the oldid matches.
   */
  virtual void renameMetaIdRefs(std::string oldid, std::string newid);


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
   *   stream.writeAttribute( "submodel", mSubmodel );
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
Port_t *
Port_create(unsigned int level, unsigned int version,
            unsigned int pkgVersion);


LIBSBML_EXTERN
void
Port_free(Port_t * p);


LIBSBML_EXTERN
Port_t *
Port_clone(Port_t * p);


LIBSBML_EXTERN
char *
Port_getId(Port_t * p);


LIBSBML_EXTERN
char *
Port_getName(Port_t * p);


LIBSBML_EXTERN
int
Port_isSetId(Port_t * p);


LIBSBML_EXTERN
int
Port_isSetName(Port_t * p);


LIBSBML_EXTERN
int
Port_setId(Port_t * p, const char * id);


LIBSBML_EXTERN
int
Port_setName(Port_t * p, const char * name);


LIBSBML_EXTERN
int
Port_unsetId(Port_t * p);


LIBSBML_EXTERN
int
Port_unsetName(Port_t * p);


LIBSBML_EXTERN
int
Port_hasRequiredAttributes(Port_t * p);


LIBSBML_EXTERN
Port_t *
ListOfPorts_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
Port_t *
ListOfPorts_removeById(ListOf_t * lo, const char * sid);



END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Port_H__ */
