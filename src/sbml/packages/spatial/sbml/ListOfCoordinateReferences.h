/**
 * @file ListOfCoordinateReferences.h
 * @brief Definition of the ListOfCoordinateReferences class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class ListOfCoordinateReferences
 * @sbmlbrief{spatial} TODO:Definition of the ListOfCoordinateReferences class.
 */


#ifndef ListOfCoordinateReferences_H__
#define ListOfCoordinateReferences_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CoordinateReference.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfCoordinateReferences : public ListOf
{

public:

  /**
   * Creates a new ListOfCoordinateReferences using the given SBML Level,
   * Version and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfCoordinateReferences.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfCoordinateReferences.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfCoordinateReferences.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfCoordinateReferences(
                             unsigned int level =
                               SpatialExtension::getDefaultLevel(),
                             unsigned int version =
                               SpatialExtension::getDefaultVersion(),
                             unsigned int pkgVersion =
                               SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCoordinateReferences using the given
   * SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfCoordinateReferences(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfCoordinateReferences.
   *
   * @param orig the ListOfCoordinateReferences instance to copy.
   */
  ListOfCoordinateReferences(const ListOfCoordinateReferences& orig);


  /**
   * Assignment operator for ListOfCoordinateReferences.
   *
   * @param rhs the ListOfCoordinateReferences object whose values are to be
   * used as the basis of the assignment.
   */
  ListOfCoordinateReferences& operator=(const ListOfCoordinateReferences& rhs);


  /**
   * Creates and returns a deep copy of this ListOfCoordinateReferences object.
   *
   * @return a (deep) copy of this ListOfCoordinateReferences object.
   */
  virtual ListOfCoordinateReferences* clone() const;


  /**
   * Destructor for ListOfCoordinateReferences.
   */
  virtual ~ListOfCoordinateReferences();


  /**
   * Get a CoordinateReference from the ListOfCoordinateReferences.
   *
   * @param n an unsigned int representing the index of the CoordinateReference
   * to retrieve.
   *
   * @return the nth CoordinateReference in this ListOfCoordinateReferences.
   *
   * @see size()
   */
  virtual CoordinateReference* get(unsigned int n);


  /**
   * Get a CoordinateReference from the ListOfCoordinateReferences.
   *
   * @param n an unsigned int representing the index of the CoordinateReference
   * to retrieve.
   *
   * @return the nth CoordinateReference in this ListOfCoordinateReferences.
   *
   * @see size()
   */
  virtual const CoordinateReference* get(unsigned int n) const;


  /**
   * Get a CoordinateReference from the ListOfCoordinateReferences based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the CoordinateReference
   * to retrieve.
   *
   * @return the CoordinateReference in this ListOfCoordinateReferences with
   * the given id or NULL if no such CoordinateReference exists.
   *
   * @see size()
   */
  virtual CoordinateReference* get(const std::string& sid);


  /**
   * Get a CoordinateReference from the ListOfCoordinateReferences based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the CoordinateReference
   * to retrieve.
   *
   * @return the CoordinateReference in this ListOfCoordinateReferences with
   * the given id or NULL if no such CoordinateReference exists.
   *
   * @see size()
   */
  virtual const CoordinateReference* get(const std::string& sid) const;


  /**
   * Removes the nth CoordinateReference from this ListOfCoordinateReferences
   * and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the CoordinateReference
   * to remove.
   *
   * @return a pointer to the nth CoordinateReference in this
   * ListOfCoordinateReferences.
   *
   * @see size()
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual CoordinateReference* remove(unsigned int n);


  /**
   * Removes the CoordinateReference from this ListOfCoordinateReferences based
   * on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the CoordinateReference
   * to remove.
   *
   * @return the CoordinateReference in this ListOfCoordinateReferences based
   * on the identifier or NULL if no such CoordinateReference exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual CoordinateReference* remove(const std::string& sid);


  /**
   * Adds a copy of the given CoordinateReference to this
   * ListOfCoordinateReferences.
   *
   * @param cr the CoordinateReference object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createCoordinateReference()
   */
  int addCoordinateReference(const CoordinateReference* cr);


  /**
   * Get the number of CoordinateReference objects in this
   * ListOfCoordinateReferences.
   *
   * @return the number of CoordinateReference objects in this
   * ListOfCoordinateReferences.
   */
  unsigned int getNumCoordinateReferences() const;


  /**
   * Creates a new CoordinateReference object, adds it to this
   * ListOfCoordinateReferences object and returns the CoordinateReference
   * object created.
   *
   * @return a new CoordinateReference object instance.
   *
   * @see addCoordinateReference(const CoordinateReference* cr)
   */
  CoordinateReference* createCoordinateReference();


  /**
   * Returns the XML element name of this ListOfCoordinateReferences object.
   *
   * For ListOfCoordinateReferences, the XML element name is always @c
   * "listOfCoordinateReferences".
   *
   * @return the name of this element, i.e. @c "listOfCoordinateReferences".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfCoordinateReferences object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_LIST_OF, SBMLTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   */
  virtual int getTypeCode() const;


  /**
   * Returns the libSBML type code for the SBML objects contained in this
   * ListOfCoordinateReferences object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfCoordinateReferences:
   *
   * @sbmlconstant{SBML_SPATIAL_COORDINATEREFERENCE, SBMLSpatialTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;




  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new CoordinateReference in this ListOfCoordinateReferences
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Spatial package
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Get a CoordinateReference_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CoordinateReference_t
 * to retrieve.
 *
 * @return the nth CoordinateReference_t in this ListOf_t.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
const CoordinateReference_t*
ListOfCoordinateReferences_getCoordinateReference(ListOf_t* lo,
                                                  unsigned int n);


/**
 * Get a CoordinateReference_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CoordinateReference_t
 * to retrieve.
 *
 * @return the CoordinateReference_t in this ListOf_t with the given id or NULL
 * if no such CoordinateReference_t exists.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
const CoordinateReference_t*
ListOfCoordinateReferences_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth CoordinateReference_t from this ListOf_t and returns a
 * pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CoordinateReference_t
 * to remove.
 *
 * @return a pointer to the nth CoordinateReference_t in this ListOf_t.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateReference_t*
ListOfCoordinateReferences_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the CoordinateReference_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CoordinateReference_t
 * to remove.
 *
 * @return the CoordinateReference_t in this ListOf_t based on the identifier
 * or NULL if no such CoordinateReference_t exists.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateReference_t*
ListOfCoordinateReferences_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfCoordinateReferences_H__ */


