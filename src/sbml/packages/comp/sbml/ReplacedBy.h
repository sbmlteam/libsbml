/**
 * @file    ReplacedBy.h
 * @brief   Definition of ReplacedBy, the Replacing-derived class of the comp package.
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
 * @class ReplacedBy
 * @ingroup Comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %ReplacedBy construct from the 'comp' package.
 *
 * The ReplacedBy class was introduced by the SBML Level&nbsp;3 
 * @ref Comp "Hierarchical Model Composition" package ('comp')
 * to allow submodel elements to be
 * 'canonical' versions of the element while still allowing the parent model
 * to reference those elements.  Whereas a ReplacedElement object indicates
 * that the containing object replaces another, a ReplacedBy object indicates
 * the converse: the parent object is to be replaced by another object.

 * As is the case with ReplacedElement, the ReplacedBy class inherits from SBaseRef.  
 * It additionally defines one required attribute ("submodelRef"), defined in 
 * libSBML in the Replacing class.
 */

#ifndef ReplacedBy_H__
#define ReplacedBy_H__


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


class LIBSBML_EXTERN ReplacedBy : public Replacing
{
public:

  /**
   * Creates a new ReplacedBy with the given level, version, and package
   * version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  ReplacedBy(unsigned int level      = CompExtension::getDefaultLevel(),
             unsigned int version    = CompExtension::getDefaultVersion(),
             unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new ReplacedBy with the given CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  ReplacedBy(CompPkgNamespaces* compns);


  /**
   * Copy constructor.
   */
  ReplacedBy(const ReplacedBy& source);


  /**
   * Assignment operator.
   */
  ReplacedBy& operator=(const ReplacedBy& source);


  /**
   * Creates and returns a deep copy of this ReplacedBy object.
   * 
   * @return a (deep) copy of this ReplacedBy object
   */
  virtual ReplacedBy* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~ReplacedBy ();


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


  /**
   * Finds this ReplacedBy's SBase parent, gets the 'comp' plugin from it,
   * and tells that to remove this.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int removeFromParentAndDelete();


  /**
   * Removes the redundant element from instantiated submodels, and points
   * all old references to the remaining element.
   */
  virtual int performReplacement();


  /** @cond doxygen-libsbml-internal */

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond */

};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
ReplacedBy_t *
ReplacedBy_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion);


LIBSBML_EXTERN
void
ReplacedBy_free(ReplacedBy_t * rb);


LIBSBML_EXTERN
ReplacedBy_t *
ReplacedBy_clone(ReplacedBy_t * rb);


LIBSBML_EXTERN
char *
ReplacedBy_getSubmodelRef(ReplacedBy_t * rb);


LIBSBML_EXTERN
int
ReplacedBy_isSetSubmodelRef(ReplacedBy_t * rb);


LIBSBML_EXTERN
int
ReplacedBy_setSubmodelRef(ReplacedBy_t * rb, const char * submodelRef);


LIBSBML_EXTERN
int
ReplacedBy_unsetSubmodelRef(ReplacedBy_t * rb);


LIBSBML_EXTERN
int
ReplacedBy_hasRequiredAttributes(ReplacedBy_t * rb);



END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ReplacedElement_H__ */
