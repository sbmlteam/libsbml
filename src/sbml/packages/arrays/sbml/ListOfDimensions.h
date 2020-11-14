/**
 * @file ListOfDimensions.h
 * @brief Definition of the ListOfDimensions class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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
 * @class ListOfDimensions
 * @sbmlbrief{arrays} TODO:Definition of the ListOfDimensions class.
 */


#ifndef ListOfDimensions_H__
#define ListOfDimensions_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/packages/arrays/sbml/Dimension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfDimensions : public ListOf
{

public:

  /**
   * Creates a new ListOfDimensions using the given SBML Level, Version and
   * &ldquo;arrays&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfDimensions.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfDimensions.
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to
   * this ListOfDimensions.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfDimensions(unsigned int level = ArraysExtension::getDefaultLevel(),
                   unsigned int version = ArraysExtension::getDefaultVersion(),
                   unsigned int pkgVersion =
                     ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDimensions using the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfDimensions(ArraysPkgNamespaces *arraysns);


  /**
   * Copy constructor for ListOfDimensions.
   *
   * @param orig the ListOfDimensions instance to copy.
   */
  ListOfDimensions(const ListOfDimensions& orig);


  /**
   * Assignment operator for ListOfDimensions.
   *
   * @param rhs the ListOfDimensions object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfDimensions& operator=(const ListOfDimensions& rhs);


  /**
   * Creates and returns a deep copy of this ListOfDimensions object.
   *
   * @return a (deep) copy of this ListOfDimensions object.
   */
  virtual ListOfDimensions* clone() const;


  /**
   * Destructor for ListOfDimensions.
   */
  virtual ~ListOfDimensions();


  /**
   * Get a Dimension from the ListOfDimensions.
   *
   * @param n an unsigned int representing the index of the Dimension to
   * retrieve.
   *
   * @return the nth Dimension in this ListOfDimensions.
   *
   * @see size()
   */
  virtual Dimension* get(unsigned int n);


  /**
   * Get a Dimension from the ListOfDimensions.
   *
   * @param n an unsigned int representing the index of the Dimension to
   * retrieve.
   *
   * @return the nth Dimension in this ListOfDimensions.
   *
   * @see size()
   */
  virtual const Dimension* get(unsigned int n) const;


  /**
   * Get a Dimension from the ListOfDimensions based on its identifier.
   *
   * @param sid a string representing the identifier of the Dimension to
   * retrieve.
   *
   * @return the Dimension in this ListOfDimensions with the given id or NULL
   * if no such Dimension exists.
   *
   * @see size()
   */
  virtual Dimension* get(const std::string& sid);


  /**
   * Get a Dimension from the ListOfDimensions based on its identifier.
   *
   * @param sid a string representing the identifier of the Dimension to
   * retrieve.
   *
   * @return the Dimension in this ListOfDimensions with the given id or NULL
   * if no such Dimension exists.
   *
   * @see size()
   */
  virtual const Dimension* get(const std::string& sid) const;


  /**
   * Removes the nth Dimension from this ListOfDimensions and returns a pointer
   * to it.
   *
   * @param n an unsigned int representing the index of the Dimension to
   * remove.
   *
   * @return a pointer to the nth Dimension in this ListOfDimensions.
   *
   * @see size()
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual Dimension* remove(unsigned int n);


  /**
   * Removes the Dimension from this ListOfDimensions based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the Dimension to
   * remove.
   *
   * @return the Dimension in this ListOfDimensions based on the identifier or
   * NULL if no such Dimension exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual Dimension* remove(const std::string& sid);


  /**
   * Adds a copy of the given Dimension to this ListOfDimensions.
   *
   * @param d the Dimension object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createDimension()
   */
  int addDimension(const Dimension* d);


  /**
   * Get the number of Dimension objects in this ListOfDimensions.
   *
   * @return the number of Dimension objects in this ListOfDimensions.
   */
  unsigned int getNumDimensions() const;


  /**
   * Creates a new Dimension object, adds it to this ListOfDimensions object
   * and returns the Dimension object created.
   *
   * @return a new Dimension object instance.
   *
   * @see addDimension(const Dimension* d)
   */
  Dimension* createDimension();


  /**
   * Get a Dimension from the ListOfDimensions based on the Size to which it
   * refers.
   *
   * @param sid a string representing the size attribute of the Dimension
   * object to retrieve.
   *
   * @return the first Dimension in this ListOfDimensions based on the given
   * size attribute or NULL if no such Dimension exists.
   */
  const Dimension* getBySize(const std::string& sid) const;


  /**
   * Get a Dimension from the ListOfDimensions based on the Size to which it
   * refers.
   *
   * @param sid a string representing the size attribute of the Dimension
   * object to retrieve.
   *
   * @return the first Dimension in this ListOfDimensions based on the given
   * size attribute or NULL if no such Dimension exists.
   */
  Dimension* getBySize(const std::string& sid);


  /**
  * Get a Dimension from the ListOfDimensions based on the ArrayDimension to which it
  * refers.
  *
  * @param sid a string representing the arrayDimension attribute of the Dimension
  * object to retrieve.
  *
  * @return the first Dimension in this ListOfDimensions based on the given
  * arrayDimension attribute or NULL if no such Dimension exists.
  */
  const Dimension* getByArrayDimension(unsigned int arrayDimension) const;


  /**
  * Get a Dimension from the ListOfDimensions based on the ArrayDimension to which it
  * refers.
  *
  * @param sid a string representing the arrayDimension attribute of the Dimension
  * object to retrieve.
  *
  * @return the first Dimension in this ListOfDimensions based on the given
  * arrayDimension attribute or NULL if no such Dimension exists.
  */
  Dimension* getByArrayDimension(unsigned int arrayDimension);


  /**
   * Returns the XML element name of this ListOfDimensions object.
   *
   * For ListOfDimensions, the XML element name is always @c
   * "listOfDimensions".
   *
   * @return the name of this element, i.e. @c "listOfDimensions".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfDimensions object.
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
   * ListOfDimensions object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfDimensions:
   *
   * @sbmlconstant{SBML_ARRAYS_DIMENSION, SBMLArraysTypeCode_t}
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
   * Creates a new Dimension in this ListOfDimensions
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Arrays package
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
 * Get a Dimension_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Dimension_t to
 * retrieve.
 *
 * @return the nth Dimension_t in this ListOf_t.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
const Dimension_t*
ListOfDimensions_getDimension(ListOf_t* lo, unsigned int n);


/**
 * Get a Dimension_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Dimension_t to
 * retrieve.
 *
 * @return the Dimension_t in this ListOf_t with the given id or NULL if no
 * such Dimension_t exists.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
const Dimension_t*
ListOfDimensions_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth Dimension_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Dimension_t to
 * remove.
 *
 * @return a pointer to the nth Dimension_t in this ListOf_t.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
Dimension_t*
ListOfDimensions_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the Dimension_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Dimension_t to
 * remove.
 *
 * @return the Dimension_t in this ListOf_t based on the identifier or NULL if
 * no such Dimension_t exists.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
Dimension_t*
ListOfDimensions_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfDimensions_H__ */


