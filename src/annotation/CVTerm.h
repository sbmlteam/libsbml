/**
 * @file    CVTerm.h
 * @brief   Definition of a CVTerm class for adding annotations to a Model.
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class CVTerm.
 * @brief Representation of MIRIAM-compliant controlled vocabulary annotation.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * CVTerm is a libSBML construct used as part of the libSBML support for
 * annotations conforming to the guidelines specified by MIRIAM ("Minimum
 * Information Requested in the Annotation of biochemical Models").  The
 * general scheme is as follows.  A set of RDF-based annotations attached
 * to a given SBML <code>&lt;annotation&gt;</code> element are read by
 * RDFAnnotationParser and converted into a list of CVTerm objects.  Each
 * CVTerm object instance stores the following components of an annotation:
 * 
 * <ul>
 * <li>The qualifier, which can be a MIRIAM "biological qualifier", a
 * "model qualifier", or an unknown qualifier (as far as the CVTerm class
 * is concerned).  Qualifiers are used in MIRIAM to indicate the nature of
 * the relationship between the object being annotated and the resource.
 * In CVTerm, the qualifiers can be manipulated using the methods
 * CVTerm::getQualifierType(),
 * CVTerm::setQualifierType(@if java int type@endif),
 * and related methods.
 * 
 * <li>The resource, represent by a URI (note: not a URL).  In CVTerm, the
 * resource component can be manipulated using the methods
 * CVTerm::addResource(@if java String resource@endif)
 * and CVTerm::removeResource(@if java String resource@endif).
 * </ul>
 */

#ifndef CVTerm_h
#define CVTerm_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/common/operationReturnValues.h>

#include <sbml/xml/XMLAttributes.h>

LIBSBML_CPP_NAMESPACE_BEGIN

typedef enum
{
    MODEL_QUALIFIER
  , BIOLOGICAL_QUALIFIER
  , UNKNOWN_QUALIFIER
} QualifierType_t;

typedef enum
{
    BQM_IS
  , BQM_IS_DESCRIBED_BY
  , BQM_IS_DERIVED_FROM
  , BQM_UNKNOWN
} ModelQualifierType_t;

typedef enum
{
    BQB_IS
  , BQB_HAS_PART
  , BQB_IS_PART_OF
  , BQB_IS_VERSION_OF
  , BQB_HAS_VERSION
  , BQB_IS_HOMOLOG_TO
  , BQB_IS_DESCRIBED_BY
  , BQB_IS_ENCODED_BY
  , BQB_ENCODES
  , BQB_OCCURS_IN
  , BQB_HAS_PROPERTY
  , BQB_IS_PROPERTY_OF
  , BQB_UNKNOWN
} BiolQualifierType_t;

LIBSBML_CPP_NAMESPACE_END

#ifdef __cplusplus


#include <limits>
#include <iomanip>
#include <string>
#include <sstream>

#include <cstdlib>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN CVTerm
{
public:

  /**
   * Creates a new CVTerm, optionally with the given @if clike #QualifierType_t@endif@if java qualifier type code@endif value
   * @p type.
   *
   * The possible qualifier types are @link QualifierType_t#MODEL_QUALIFIER MODEL_QUALIFIER@endlink 
   * and @link QualifierType_t#BIOLOGICAL_QUALIFIER BIOLOGICAL_QUALIFIER@endlink.  If an explicit
   * value for @p type is not given, this method defaults to using
   * @link QualifierType_t#UNKNOWN_QUALIFIER UNKNOWN_QUALIFIER@endlink.  The
   * @if clike #QualifierType_t@endif@if java qualifier type code@endif type
   * value can be set later using the CVTerm::setQualifierType(@if java int type@endif)
   * method.
   *
   * @param type a @if clike #QualifierType_t@endif@if java qualifier type code@endif value
   *
   * @if notcpp @docnote @htmlinclude warn-default-args-in-docs.html @endif
   */
  CVTerm(QualifierType_t type = UNKNOWN_QUALIFIER);


  /**
   * Create a new CVTerm from the given XMLNode.
   *
   * In libSBML, RDF annotations on a model component are stored as a list
   * of CVTerm ("controlled vocabulary term") objects.  This allows the
   * user to interact with the CVTerm objects directly.  When LibSBML reads
   * in a model containing RDFAnnotation, it parses them into a list of
   * CVTerm objects, and when writing a model, it parses the CVTerm objects
   * into the appropriate SBML <code>&lt;annotation&gt;</code> structure.
   * 
   * This method creates a CVTerm from the XMLNode object supplied.
   *
   * @param node an %XMLNode representing a CVTerm.
   *
   * @note This method assumes that the given XMLNode object @p node is of
   * the correct structural form.
   */
  CVTerm(const XMLNode node);


  /**
   * Destroys this CVTerm.
   */
  ~CVTerm();


  /**
   * Copy constructor; creates a copy of a CVTerm.
   * 
   * @param orig the CVTerm instance to copy.
   */
  CVTerm(const CVTerm& orig);


  /**
   * Assignment operator for CVTerm.
   */
  CVTerm& operator=(const CVTerm& rhs);


  /**
   * Creates and returns a deep copy of this CVTerm object.
   * 
   * @return a (deep) copy of this CVTerm.
   */  
  CVTerm* clone() const; 


  /**
   * Returns the qualifier type code for this CVTerm object.
   * 
   * @return the @if clike #QualifierType_t@endif@if java qualifier type code@endif value of this object or
   * @link QualifierType_t#UNKNOWN_QUALIFIER UNKNOWN_QUALIFIER@endlink
   * (the default).
   */
  QualifierType_t getQualifierType();


  /**
   * Returns the model qualifier type code for this CVTerm object.
   * 
   * @return the @if clike #ModelQualifierType_t@endif@if java model qualifier type code@endif value of this object or
   * @link ModelQualifierType_t#BQM_UNKNOWN BQM_UNKNOWN@endlink
   * (the default).
   */
  ModelQualifierType_t getModelQualifierType();


  /**
   * Returns the biological qualifier type code for this CVTerm object.
   * 
   * @return the @if clike #BiolQualifierType_t@endif@if java biology qualifier type code@endif value of this object or
   * @link BiolQualifierType_t#BQB_UNKNOWN BQB_UNKNOWN@endlink
   * (the default).
   */
  BiolQualifierType_t getBiologicalQualifierType();


  /**
   * Returns the resources for this CVTerm object.
   * 
   * @return the XMLAttributes that store the resources of this CVTerm.
   */
  XMLAttributes * getResources(); 

  
  /**
   * Returns the resources for this CVTerm object.
   * 
   * @return the XMLAttributes that store the resources of this CVTerm.
   */
  const XMLAttributes * getResources() const; 

  
  /**
   * Returns the number of resources for this CVTerm object.
   * 
   * @return the number of resources in the set of XMLAttributes
   * of this CVTerm.
   */
  unsigned int getNumResources(); 

  
  /**
   * Returns the value of the nth resource for this CVTerm object.
   *
   * @param n the index of the resource to query
   *
   * @return string representing the value of the nth resource
   * in the set of XMLAttributes of this CVTerm.
   *
   * @note Since the values of the resource attributes in a CVTerm are
   * URIs, this is a convenience method to facilitate interaction with the
   * CVTerm class.
   */
  const std::string& getResourceURI(unsigned int n) const; 

  
  /**
   * Sets the @if clike #QualifierType_t@endif@if java qualifier type code@endif value of this CVTerm object.
   *
   * @param type the @if clike #QualifierType_t@endif@if java qualifier type code@endif type value 
   * function. The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
   */
  int setQualifierType(QualifierType_t type);


  /**
   * Sets the @if clike #ModelQualifierType_t@endif@if java model qualifier type code@endif value of this CVTerm object.
   *
   * @param type the @if clike #ModelQualifierType_t@endif@if java model qualifier type code@endif value
   *
   * @return integer value indicating success/failure of the
   * function. The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
   *
   * @note If the Qualifier Type of this object is not
   * @link QualifierType_t#MODEL_QUALIFIER MODEL_QUALIFIER@endlink, 
   * then the ModelQualifierType_t value will default to
   * @link QualifierType_t#BQM_UNKNOWN BQM_UNKNOWN@endlink.
   */
  int setModelQualifierType(ModelQualifierType_t type);


  /**
   * Sets the @if clike #BiolQualifierType_t@endif@if java biology qualifier type code@endif of this CVTerm object.
   *
   * @param type the @if clike #BiolQualifierType_t@endif@if java biology qualifier type code@endif value
   *
   * @return integer value indicating success/failure of the
   * function. The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
   *
   * @note If the Qualifier Type of this object is not
   * @link QualifierType_t#BIOLOGICAL_QUALIFIER BIOLOGICAL_QUALIFIER@endlink,
   * then the @if clike #BiolQualifierType_t@endif@if java biology qualifier type code@endif value will default
   * to @link BiolQualifierType_t#BQB_UNKNOWN BQB_UNKNOWN@endlink.
   */
  int setBiologicalQualifierType(BiolQualifierType_t type);


  /**
   * Adds a resource to the CVTerm.
   *
   * @param resource string representing the resource; e.g.,
   * <code>"http://www.geneontology.org/#GO:0005892"</code>.
   *
   * @note This method adds the name <code>"rdf:resource"</code> to the
   * attribute prior to adding it to the resources in this CVTerm object.
   *
   * @return integer value indicating success/failure of the
   * function. The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
   */
  int addResource(const std::string& resource);


  /**
   * Removes a resource from the CVTerm.
   *
   * @param resource string representing the resource; e.g.,
   * <code>"http://www.geneontology.org/#GO:0005892"</code>.
   *
   * @return integer value indicating success/failure of the
   * function. The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
   */
  int removeResource(std::string resource);
  

  /**
   * Predicate returning @c true if all the
   * required elements for this Rule object have been set.
   *
   * @note The required attributes for a CVTerm are:
   * @li qualifierType and appropriate biologicalQualifierType or 
   * @li modelQualifierType and at least one resource.
   */ 
  bool hasRequiredAttributes();


protected:
  /** @cond doxygen-libsbml-internal */

  XMLAttributes * mResources;

  QualifierType_t       mQualifier;
  ModelQualifierType_t  mModelQualifier;
  BiolQualifierType_t   mBiolQualifier;

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/

LIBSBML_EXTERN
CVTerm_t*
CVTerm_createWithQualifierType(QualifierType_t type);


LIBSBML_EXTERN
CVTerm_t*
CVTerm_createFromNode(const XMLNode_t *);


LIBSBML_EXTERN
void
CVTerm_free(CVTerm_t *);


LIBSBML_EXTERN
CVTerm_t *
CVTerm_clone (const CVTerm_t* c);


LIBSBML_EXTERN
QualifierType_t 
CVTerm_getQualifierType(CVTerm_t *);


LIBSBML_EXTERN
ModelQualifierType_t 
CVTerm_getModelQualifierType(CVTerm_t *);


LIBSBML_EXTERN
BiolQualifierType_t 
CVTerm_getBiologicalQualifierType(CVTerm_t *);


LIBSBML_EXTERN
XMLAttributes_t * 
CVTerm_getResources(CVTerm_t *); 


LIBSBML_EXTERN
unsigned int
CVTerm_getNumResources(CVTerm_t*);


LIBSBML_EXTERN
char *
CVTerm_getResourceURI(CVTerm_t * cv, unsigned int n);


LIBSBML_EXTERN
int 
CVTerm_setQualifierType(CVTerm_t * CVT, QualifierType_t type);


LIBSBML_EXTERN
int 
CVTerm_setModelQualifierType(CVTerm_t * CVT, ModelQualifierType_t type);


LIBSBML_EXTERN
int 
CVTerm_setBiologicalQualifierType(CVTerm_t * CVT, BiolQualifierType_t type);


LIBSBML_EXTERN
int 
CVTerm_addResource(CVTerm_t * CVT, const char * resource);


LIBSBML_EXTERN
int 
CVTerm_removeResource(CVTerm_t * CVT, const char * resource);


LIBSBML_EXTERN
int
CVTerm_hasRequiredAttributes(CVTerm_t *cvt);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /** CVTerm_h **/
