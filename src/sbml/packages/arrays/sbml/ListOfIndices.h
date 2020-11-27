/**
 * @file ListOfIndices.h
 * @brief Definition of the ListOfIndices class.
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
 * @class ListOfIndices
 * @sbmlbrief{arrays} TODO:Definition of the ListOfIndices class.
 */


#ifndef ListOfIndices_H__
#define ListOfIndices_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/packages/arrays/sbml/Index.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfIndices : public ListOf
{

public:

  /**
   * Creates a new ListOfIndices using the given SBML Level, Version and
   * &ldquo;arrays&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfIndices.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfIndices.
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to
   * this ListOfIndices.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfIndices(unsigned int level = ArraysExtension::getDefaultLevel(),
                unsigned int version = ArraysExtension::getDefaultVersion(),
                unsigned int pkgVersion =
                  ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfIndices using the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfIndices(ArraysPkgNamespaces *arraysns);


  /**
   * Copy constructor for ListOfIndices.
   *
   * @param orig the ListOfIndices instance to copy.
   */
  ListOfIndices(const ListOfIndices& orig);


  /**
   * Assignment operator for ListOfIndices.
   *
   * @param rhs the ListOfIndices object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfIndices& operator=(const ListOfIndices& rhs);


  /**
   * Creates and returns a deep copy of this ListOfIndices object.
   *
   * @return a (deep) copy of this ListOfIndices object.
   */
  virtual ListOfIndices* clone() const;


  /**
   * Destructor for ListOfIndices.
   */
  virtual ~ListOfIndices();


  /**
   * Get an Index from the ListOfIndices.
   *
   * @param n an unsigned int representing the index of the Index to retrieve.
   *
   * @return the nth Index in this ListOfIndices.
   *
   * @see size()
   */
  virtual Index* get(unsigned int n);


  /**
   * Get an Index from the ListOfIndices.
   *
   * @param n an unsigned int representing the index of the Index to retrieve.
   *
   * @return the nth Index in this ListOfIndices.
   *
   * @see size()
   */
  virtual const Index* get(unsigned int n) const;


  /**
   * Get an Index from the ListOfIndices based on its identifier.
   *
   * @param sid a string representing the identifier of the Index to retrieve.
   *
   * @return the Index in this ListOfIndices with the given id or NULL if no
   * such Index exists.
   *
   * @see size()
   */
  virtual Index* get(const std::string& sid);


  /**
   * Get an Index from the ListOfIndices based on its identifier.
   *
   * @param sid a string representing the identifier of the Index to retrieve.
   *
   * @return the Index in this ListOfIndices with the given id or NULL if no
   * such Index exists.
   *
   * @see size()
   */
  virtual const Index* get(const std::string& sid) const;


  /**
   * Removes the nth Index from this ListOfIndices and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the Index to remove.
   *
   * @return a pointer to the nth Index in this ListOfIndices.
   *
   * @see size()
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual Index* remove(unsigned int n);


  /**
   * Removes the Index from this ListOfIndices based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the Index to remove.
   *
   * @return the Index in this ListOfIndices based on the identifier or NULL if
   * no such Index exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual Index* remove(const std::string& sid);


  /**
   * Adds a copy of the given Index to this ListOfIndices.
   *
   * @param i the Index object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createIndex()
   */
  int addIndex(const Index* i);


  /**
   * Get the number of Index objects in this ListOfIndices.
   *
   * @return the number of Index objects in this ListOfIndices.
   */
  unsigned int getNumIndices() const;


  /**
   * Creates a new Index object, adds it to this ListOfIndices object and
   * returns the Index object created.
   *
   * @return a new Index object instance.
   *
   * @see addIndex(const Index* i)
   */
  Index* createIndex();


  /**
  * Get a Index from the ListOfIndices based on the ArrayDimension to which it
  * refers.
  *
  * @param sid a string representing the arrayDimension attribute of the Index
  * object to retrieve.
  *
  * @return the first Index in this ListOfIndices based on the given
  * arrayDimension attribute or NULL if no such Index exists.
  */
  const Index* getByArrayDimension(unsigned int arrayDimension) const;


  /**
  * Get a Index from the ListOfIndices based on the ArrayDimension to which it
  * refers.
  *
  * @param sid a string representing the arrayDimension attribute of the Index
  * object to retrieve.
  *
  * @return the first Index in this ListOfIndices based on the given
  * arrayDimension attribute or NULL if no such Index exists.
  */
  Index* getByArrayDimension(unsigned int arrayDimension);


  /**
   * Returns the XML element name of this ListOfIndices object.
   *
   * For ListOfIndices, the XML element name is always @c "listOfIndices".
   *
   * @return the name of this element, i.e. @c "listOfIndices".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfIndices object.
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
   * ListOfIndices object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this ListOfIndices:
   *
   * @sbmlconstant{SBML_ARRAYS_INDEX, SBMLArraysTypeCode_t}
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
   * Creates a new Index in this ListOfIndices
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
 * Get an Index_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Index_t to retrieve.
 *
 * @return the nth Index_t in this ListOf_t.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
const Index_t*
ListOfIndices_getIndex(ListOf_t* lo, unsigned int n);


/**
 * Get an Index_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Index_t to retrieve.
 *
 * @return the Index_t in this ListOf_t with the given id or NULL if no such
 * Index_t exists.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
const Index_t*
ListOfIndices_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth Index_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Index_t to remove.
 *
 * @return a pointer to the nth Index_t in this ListOf_t.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
Index_t*
ListOfIndices_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the Index_t from this ListOf_t based on its identifier and returns a
 * pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Index_t to remove.
 *
 * @return the Index_t in this ListOf_t based on the identifier or NULL if no
 * such Index_t exists.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
Index_t*
ListOfIndices_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfIndices_H__ */


