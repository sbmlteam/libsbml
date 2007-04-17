/**
 * @file    CVTerm.h
 * @brief   Definition of a CVTerm class for adding annotations to a Model.
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
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
 * @brief LibSBML implementation of a CVTerm construct.
 *
 * A <em>CV term</em> is
 */

#ifndef CVTerm_h
#define CVTerm_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>

#include <sbml/xml/XMLAttributes.h>

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
  , BQB_UNKNOWN
} BiolQualifierType_t;

#ifdef __cplusplus


#include <limits>
#include <iomanip>
#include <string>
#include <sstream>

#include <cstdlib>



class LIBSBML_EXTERN CVTerm
{
public:

  /**
   * Creates a new CVTerm, optionally with the given @p QualifierType.
   *
   * The possible QualifierTypes are MODEL_QUALIFIER and BIOLOGICAL_QUALIFIER.  
   * If the QualifierType is not set it will default to UNKNOWN_QUALIFIER.  The
   * QualifierType can be set using the setQualifierType() method.
   *
   * @param type a QualifierType_t
   */
  CVTerm(QualifierType_t type = UNKNOWN_QUALIFIER);

  /**
   * Create a new CVTerm from the given XMLNode.
   *
   * RDFAnnotations within a model are stored as a List of CVTerms.  This allows
   * the user to interact with the %CVTerms directly.  When LibSBML reads in a 
   * model containing RDFAnnotations it parses them into a %List of CVTerms and
   * when writing a model it parses the CVTerms into the appropriate annotation
   * structure.  This function creates a %CVTerm from the %XMLNode supplied.
   *
   * @param node an %XMLNode representing a %CVTerm.
   *
   * @note this method assumes that the %XMLNode is of the correct form
   */
  CVTerm(const XMLNode node);

  /**
   * Destroys this CVTerm.
   */
  ~CVTerm();

  /**
   * Creates and returns a deep copy of this CVTerm.
   * 
   * @return a (deep) copy of this CVTerm.
   */  
  CVTerm* clone() const; 

  /**
   * Returns the Qualifier Type code for this %CVTerm.
   * 
   * @return the QualifierType_t of this object or UNKNOWN_QUALIFIER (default).
   */
  QualifierType_t getQualifierType();

  /**
   * Returns the Model QualifierType code for this %CVTerm.
   * 
   * @return the ModelQualifierType_t of this object or BQM_UNKNOWN (default).
   */
  ModelQualifierType_t getModelQualifierType();

  /**
   * Returns the Biological QualifierType code for this %CVTerm.
   * 
   * @return the BiolQualifierType_t of this object or BQB_UNKNOWN (default).
   */
  BiolQualifierType_t getBiologicalQualifierType();

  /**
   * Returns the resources for this %CVTerm.
   * 
   * @return the XMLAttributes that store the resources of this %CVTerm.
   */
  XMLAttributes * getResources(); 
  
  /**
   * Sets the "QualifierType_t" of this %CVTerm.
   *
   * @param type the QualifierType_t 
   */
  void setQualifierType(QualifierType_t type);

  /**
   * Sets the "ModelQualifierType_t" of this %CVTerm.
   *
   * @param type the ModelQualifierType_t
   *
   * @note if the QualifierType for this object is not MODEL_QUALIFIER
   * then the ModelQualifierType will default to BQM_UNKNOWN.
   */
  void setModelQualifierType(ModelQualifierType_t type);

  /**
   * Sets the "BiolQualifierType_t" of this %CVTerm.
   *
   * @param type the BiolQualifierType_t
   *
   * @note if the QualifierType for this object is not BIOLOGICAL_QUALIFIER
   * then the BiolQualifierType_t will default to BQB_UNKNOWN.
   */
  void setBiologicalQualifierType(BiolQualifierType_t type);

  /**
   * Adds a resource to the CVTerm.
   *
   * @param resource string representing the resource 
   * e.g. http://www.geneontology.org/#GO:0005892
   *
   * @note this method adds the name "rdf:resource" to the attribute prior
   * to adding it to the resources in this CVTerm.
   */
  void addResource(std::string resource);

protected:

  XMLAttributes * mResources;

  QualifierType_t       mQualifier;
  ModelQualifierType_t  mModelQualifier;
  BiolQualifierType_t   mBiolQualifier;

};



#endif  /* __cplusplus */

#ifndef SWIG

BEGIN_C_DECLS

/**
 *
 */
LIBSBML_EXTERN
CVTerm_t*
CVTerm_createWithQualifierType(QualifierType_t type);

/**
 * 
 */
LIBSBML_EXTERN
CVTerm_t*
CVTerm_createFromNode(const XMLNode_t *);

/**
 *
 */
LIBSBML_EXTERN
void
CVTerm_free(CVTerm_t *);

/**
 * gets the Qualifier type
 */
LIBSBML_EXTERN
QualifierType_t 
CVTerm_getQualifierType(CVTerm_t *);

/**
  * gets the Model Qualifier type
  */
LIBSBML_EXTERN
ModelQualifierType_t 
CVTerm_getModelQualifierType(CVTerm_t *);

/**
  * gets the biological Qualifier type
  */
LIBSBML_EXTERN
BiolQualifierType_t 
CVTerm_getBiologicalQualifierType(CVTerm_t *);

/**
* gets the resources
*/
LIBSBML_EXTERN
XMLAttributes_t * 
CVTerm_getResources(CVTerm_t *); 

/**
  * set the qualifier type
  */
LIBSBML_EXTERN
void 
CVTerm_setQualifierType(CVTerm_t * CVT, QualifierType_t type);

/**
  * set the model qualifier type
  * this should be consistent with the mQualifier == MODEL_QUALIFIER
  */
LIBSBML_EXTERN
void 
CVTerm_setModelQualifierType(CVTerm_t * CVT, ModelQualifierType_t type);

/**
  * set the biological qualifier type
  * this should be consistent with the mQualifier == BIOLOGICAL_QUALIFIER
  */
LIBSBML_EXTERN
void 
CVTerm_setBiologicalQualifierType(CVTerm_t * CVT, BiolQualifierType_t type);

/**
  * adds a resource to the term
  */
LIBSBML_EXTERN
void 
CVTerm_addResource(CVTerm_t * CVT, const char * resource);



END_C_DECLS

#endif  /* !SWIG */

#endif  /** CVTerm_h **/
