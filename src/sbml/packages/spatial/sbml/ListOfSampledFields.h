/**
 * @file ListOfSampledFields.h
 * @brief Definition of the ListOfSampledFields class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class ListOfSampledFields
 * @sbmlbrief{spatial} TODO:Definition of the ListOfSampledFields class.
 */


#ifndef ListOfSampledFields_H__
#define ListOfSampledFields_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/SampledField.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfSampledFields : public ListOf
{

public:

  /**
   * Creates a new ListOfSampledFields using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfSampledFields.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfSampledFields.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfSampledFields.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfSampledFields(unsigned int level = SpatialExtension::getDefaultLevel(),
                      unsigned int version =
                        SpatialExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSampledFields using the given SpatialPkgNamespaces
   * object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  ListOfSampledFields(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfSampledFields.
   *
   * @param orig the ListOfSampledFields instance to copy.
   */
  ListOfSampledFields(const ListOfSampledFields& orig);


  /**
   * Assignment operator for ListOfSampledFields.
   *
   * @param rhs the ListOfSampledFields object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfSampledFields& operator=(const ListOfSampledFields& rhs);


  /**
   * Creates and returns a deep copy of this ListOfSampledFields object.
   *
   * @return a (deep) copy of this ListOfSampledFields object.
   */
  virtual ListOfSampledFields* clone() const;


  /**
   * Destructor for ListOfSampledFields.
   */
  virtual ~ListOfSampledFields();


  /**
   * Get a SampledField from the ListOfSampledFields.
   *
   * @param n an unsigned int representing the index of the SampledField to
   * retrieve.
   *
   * @return the nth SampledField in this ListOfSampledFields.
   *
   * @see size()
   */
  virtual SampledField* get(unsigned int n);


  /**
   * Get a SampledField from the ListOfSampledFields.
   *
   * @param n an unsigned int representing the index of the SampledField to
   * retrieve.
   *
   * @return the nth SampledField in this ListOfSampledFields.
   *
   * @see size()
   */
  virtual const SampledField* get(unsigned int n) const;


  /**
   * Get a SampledField from the ListOfSampledFields based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledField to
   * retrieve.
   *
   * @return the SampledField in this ListOfSampledFields with the given id or
   * NULL if no such SampledField exists.
   *
   * @see size()
   */
  virtual SampledField* get(const std::string& sid);


  /**
   * Get a SampledField from the ListOfSampledFields based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledField to
   * retrieve.
   *
   * @return the SampledField in this ListOfSampledFields with the given id or
   * NULL if no such SampledField exists.
   *
   * @see size()
   */
  virtual const SampledField* get(const std::string& sid) const;


  /**
   * Removes the nth SampledField from this ListOfSampledFields and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the SampledField to
   * remove.
   *
   * @return a pointer to the nth SampledField in this ListOfSampledFields.
   *
   * @see size()
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual SampledField* remove(unsigned int n);


  /**
   * Removes the SampledField from this ListOfSampledFields based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the SampledField to
   * remove.
   *
   * @return the SampledField in this ListOfSampledFields based on the
   * identifier or NULL if no such SampledField exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  virtual SampledField* remove(const std::string& sid);


  /**
   * Adds a copy of the given SampledField to this ListOfSampledFields.
   *
   * @param sf the SampledField object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createSampledField()
   */
  int addSampledField(const SampledField* sf);


  /**
   * Get the number of SampledField objects in this ListOfSampledFields.
   *
   * @return the number of SampledField objects in this ListOfSampledFields.
   */
  unsigned int getNumSampledFields() const;


  /**
   * Creates a new SampledField object, adds it to this ListOfSampledFields
   * object and returns the SampledField object created.
   *
   * @return a new SampledField object instance.
   *
   * @see addSampledField(const SampledField* sf)
   */
  SampledField* createSampledField();


  /**
   * Returns the XML element name of this ListOfSampledFields object.
   *
   * For ListOfSampledFields, the XML element name is always @c
   * "listOfSampledFields".
   *
   * @return the name of this element, i.e. @c "listOfSampledFields".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfSampledFields object.
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
   * ListOfSampledFields object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfSampledFields:
   *
   * @sbmlconstant{SBML_SPATIAL_SAMPLEDFIELD, SBMLSpatialTypeCode_t}
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
   * Creates a new SampledField in this ListOfSampledFields
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
 * Get a SampledField_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledField_t to
 * retrieve.
 *
 * @return the nth SampledField_t in this ListOf_t.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
const SampledField_t*
ListOfSampledFields_getSampledField(ListOf_t* lo, unsigned int n);


/**
 * Get a SampledField_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledField_t to
 * retrieve.
 *
 * @return the SampledField_t in this ListOf_t with the given id or NULL if no
 * such SampledField_t exists.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
const SampledField_t*
ListOfSampledFields_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth SampledField_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledField_t to
 * remove.
 *
 * @return a pointer to the nth SampledField_t in this ListOf_t.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
SampledField_t*
ListOfSampledFields_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the SampledField_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledField_t to
 * remove.
 *
 * @return the SampledField_t in this ListOf_t based on the identifier or NULL
 * if no such SampledField_t exists.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
SampledField_t*
ListOfSampledFields_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfSampledFields_H__ */


