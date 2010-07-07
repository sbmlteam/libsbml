/**
 * @file    operationReturnValues.h
 * @brief   Enumeration of values returned by operations within libSBML.
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#ifndef LIBSBML_OPERATION_RETURN_VALUES_H
#define LIBSBML_OPERATION_RETURN_VALUES_H 1

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Diagnostic return codes.
 *
 * Many methods in libSBML return a status code to indicate whether the
 * operation requested by the caller succeeded or failed.  This enumeration
 * lists all the possible return codes from any libSBML methods.
 */
typedef enum
{
    LIBSBML_OPERATION_SUCCESS       = 0
    /*!< The operation was successful. */

  , LIBSBML_INDEX_EXCEEDS_SIZE      = -1
    /*!< An index parameter exceeded the bounds of a data array or other
     * collection used in the operation.  This return value is typically
     * returned by methods that take index numbers to refer to lists
     * of objects, when the caller has provided an index that exceeds
     * the bounds of the list.  LibSBML provides methods for checking the
     * size of list/sequence/collection structures, and callers should
     * verify the sizes before calling methods that take index numbers. */

  , LIBSBML_UNEXPECTED_ATTRIBUTE    = -2
    /*!< The attribute that is the subject of this operation is not valid
     * for the combination of SBML Level and Version for the underlying
     * object.  This can happen because libSBML strives to offer a uniform
     * API for all SBML Levels and Versions, but some object attributes and
     * elements are not defined for all SBML Levels and Versions.  Calling
     * programs are expected to be aware of which object structures they
     * are working with, but when errors of this kind occur, they are
     * reported using this return value. */

  , LIBSBML_OPERATION_FAILED        = -3
    /*!< The requested action could not be performed.  This can occur in
     * a variety of contexts, such as passing a null object as a parameter
     * in a situation where it does not make sense to permit a null object.
     */

  , LIBSBML_INVALID_ATTRIBUTE_VALUE = -4
    /*!< A value passed as an argument to the method is not of a type that
     * is valid for the operation or kind of object involved.  For example,
     * this return code is used when a calling program attempts to set an
     * SBML object identifier to a string whose syntax does not conform to
     * the SBML identifier syntax. */

  , LIBSBML_INVALID_OBJECT          = -5
    /*!< The object passed as an argument to the method is not of a type
     * that is valid for the operation or kind of object involved.  For
     * example, handing an invalidly-constructed ASTNode to a method
     * expecting an ASTNode will result in this error. */

  , LIBSBML_DUPLICATE_OBJECT_ID     = -6
    /*!< There already exists an object with this identifier in the
     * context where this operation is being attempted.  This error is
     * typically returned in situations where SBML object identifiers must be
     * unique, such as attempting to add two species with the same identifier
     * to a model. */

  , LIBSBML_LEVEL_MISMATCH          = -7
    /*!< The SBML Level associated with the object does not match the Level
     * of the parent object.  This error can happen when an SBML component
     * such as a species or compartment object is created outside of a model
     * and a calling program then attempts to add the object to a model that
     * has a different SBML Level defined. */

  , LIBSBML_VERSION_MISMATCH        = -8
    /*!< The SBML Version within the SBML Level associated with the object
     * does not match the Version of the parent object.  This error can
     * happen when an SBML component such as a species or compartment object
     * is created outside of a model and a calling program then attempts to
     * add the object to a model that has a different SBML Level+Version
     * combination. */

  , LIBSBML_INVALID_XML_OPERATION   = -9
    /*!< The XML operation attempted is not valid for the object or context
     * involved.  This error is typically returned by the XML interface layer
     * of libSBML, when a calling program attempts to construct or manipulate
     * XML in an invalid way.  */

} OperationReturnValues_t;

LIBSBML_CPP_NAMESPACE_END


#endif  /* LIBSBML_OPERATION_RETURN_VALUES_H */
