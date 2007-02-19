/**
 * \file    CVTerm.h
 * \brief   CVTerm I/O
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
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
   * Creates a new CVTerm.
   */
  CVTerm (QualifierType_t type = UNKNOWN_QUALIFIER);

  /**
   * create a new CVTerm from an XMLNode
   */
  CVTerm(const XMLNode);

  /**
   * destructor
   */
  ~CVTerm();

  /**
   * gets the Qualifier type
   */
  QualifierType_t getQualifierType();

  /**
   * gets the Model Qualifier type
   */
  ModelQualifierType_t getModelQualifierType();

  /**
   * gets the biological Qualifier type
   */
  BiolQualifierType_t getBiologicalQualifierType();

  /**
  * gets the resources
  */
  XMLAttributes * getResources(); 
  
  /**
   * set the qualifier type
   */
  void setQualifierType(QualifierType_t type);

  /**
   * set the model qualifier type
   * this should be consistent with the mQualifier == MODEL_QUALIFIER
   */
  void setModelQualifierType(ModelQualifierType_t type);

  /**
   * set the biological qualifier type
   * this should be consistent with the mQualifier == BIOLOGICAL_QUALIFIER
   */
  void setBiologicalQualifierType(BiolQualifierType_t type);

  /**
   * adds a resource to the term
   */
  void addResource(std::string resource);

protected:

  XMLAttributes * mResources;

  QualifierType_t       mQualifier;
  ModelQualifierType_t  mModelQualifier;
  BiolQualifierType_t   mBiolQualifier;

};



#endif  /* __cplusplus */

#endif  /** CVTerm_h **/
