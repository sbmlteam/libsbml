/**
 * @file ListOfOrdinalMappings.h
 * @brief Definition of the ListOfOrdinalMappings class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
 * @class ListOfOrdinalMappings
 * @sbmlbrief{spatial} TODO:Definition of the ListOfOrdinalMappings class.
 */


#ifndef ListOfOrdinalMappings_H__
#define ListOfOrdinalMappings_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/OrdinalMapping.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfOrdinalMappings : public ListOf
{

public:

  /**
   * Creates a new ListOfOrdinalMappings using the given SBML Level, Version
   * and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfOrdinalMappings.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfOrdinalMappings.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfOrdinalMappings.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfOrdinalMappings(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                        unsigned int version =
                          SpatialExtension::getDefaultVersion(),
                        unsigned int pkgVersion =
                          SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfOrdinalMappings using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfOrdinalMappings(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfOrdinalMappings.
   *
   * @param orig the ListOfOrdinalMappings instance to copy.
   */
  ListOfOrdinalMappings(const ListOfOrdinalMappings& orig);


  /**
   * Assignment operator for ListOfOrdinalMappings.
   *
   * @param rhs the ListOfOrdinalMappings object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfOrdinalMappings& operator=(const ListOfOrdinalMappings& rhs);


  /**
   * Creates and returns a deep copy of this ListOfOrdinalMappings object.
   *
   * @return a (deep) copy of this ListOfOrdinalMappings object.
   */
  virtual ListOfOrdinalMappings* clone() const;


  /**
   * Destructor for ListOfOrdinalMappings.
   */
  virtual ~ListOfOrdinalMappings();


  /**
   * Get an OrdinalMapping from the ListOfOrdinalMappings.
   *
   * @param n an unsigned int representing the index of the OrdinalMapping to
   * retrieve.
   *
   * @return the nth OrdinalMapping in this ListOfOrdinalMappings.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see get(const std::string& sid)
   * @see getNumOrdinalMappings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual OrdinalMapping* get(unsigned int n);


  /**
   * Get an OrdinalMapping from the ListOfOrdinalMappings.
   *
   * @param n an unsigned int representing the index of the OrdinalMapping to
   * retrieve.
   *
   * @return the nth OrdinalMapping in this ListOfOrdinalMappings.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see get(const std::string& sid)
   * @see getNumOrdinalMappings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const OrdinalMapping* get(unsigned int n) const;


  /**
   * Get an OrdinalMapping from the ListOfOrdinalMappings based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the OrdinalMapping to
   * retrieve.
   *
   * @return the OrdinalMapping in this ListOfOrdinalMappings with the given @p
   * sid or @c NULL if no such OrdinalMapping exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see get(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual OrdinalMapping* get(const std::string& sid);


  /**
   * Get an OrdinalMapping from the ListOfOrdinalMappings based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the OrdinalMapping to
   * retrieve.
   *
   * @return the OrdinalMapping in this ListOfOrdinalMappings with the given @p
   * sid or @c NULL if no such OrdinalMapping exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see get(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const OrdinalMapping* get(const std::string& sid) const;


  /**
   * Removes the nth OrdinalMapping from this ListOfOrdinalMappings and returns
   * a pointer to it.
   *
   * @param n an unsigned int representing the index of the OrdinalMapping to
   * remove.
   *
   * @return a pointer to the nth OrdinalMapping in this ListOfOrdinalMappings.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see remove(const std::string& sid)
   */
  virtual OrdinalMapping* remove(unsigned int n);


  /**
   * Removes the OrdinalMapping from this ListOfOrdinalMappings based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the OrdinalMapping to
   * remove.
   *
   * @return the OrdinalMapping in this ListOfOrdinalMappings based on the
   * identifier or NULL if no such OrdinalMapping exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see remove(unsigned int n)
   */
  virtual OrdinalMapping* remove(const std::string& sid);


  /**
   * Adds a copy of the given OrdinalMapping to this ListOfOrdinalMappings.
   *
   * @param om the OrdinalMapping object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createOrdinalMapping()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addOrdinalMapping(const OrdinalMapping* om);


  /**
   * Get the number of OrdinalMapping objects in this ListOfOrdinalMappings.
   *
   * @return the number of OrdinalMapping objects in this
   * ListOfOrdinalMappings.
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumOrdinalMappings() const;


  /**
   * Creates a new OrdinalMapping object, adds it to this ListOfOrdinalMappings
   * object and returns the OrdinalMapping object created.
   *
   * @return a new OrdinalMapping object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  OrdinalMapping* createOrdinalMapping();


  /**
   * Get an OrdinalMapping from the ListOfOrdinalMappings based on the
   * GeometryDefinition to which it refers.
   *
   * @param sid a string representing the "geometryDefinition" attribute of the
   * OrdinalMapping object to retrieve.
   *
   * @return the first OrdinalMapping in this ListOfOrdinalMappings based on
   * the given geometryDefinition attribute or NULL if no such OrdinalMapping
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const OrdinalMapping* getByGeometryDefinition(const std::string& sid) const;


  /**
   * Get an OrdinalMapping from the ListOfOrdinalMappings based on the
   * GeometryDefinition to which it refers.
   *
   * @param sid a string representing the "geometryDefinition" attribute of the
   * OrdinalMapping object to retrieve.
   *
   * @return the first OrdinalMapping in this ListOfOrdinalMappings based on
   * the given geometryDefinition attribute or NULL if no such OrdinalMapping
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  OrdinalMapping* getByGeometryDefinition(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfOrdinalMappings object.
   *
   * For ListOfOrdinalMappings, the XML element name is always
   * @c "listOfOrdinalMappings".
   *
   * @return the name of this element, i.e. @c "listOfOrdinalMappings".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfOrdinalMappings object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_LIST_OF, SBMLTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   */
  virtual int getTypeCode() const;


  /**
   * Returns the libSBML type code for the SBML objects contained in this
   * ListOfOrdinalMappings object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfOrdinalMappings:
   * @sbmlconstant{SBML_SPATIAL_ORDINALMAPPING, SBMLSpatialTypeCode_t}.
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
   * Creates a new OrdinalMapping in this ListOfOrdinalMappings
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
 * Get an OrdinalMapping_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the OrdinalMapping_t to
 * retrieve.
 *
 * @return the nth OrdinalMapping_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfOrdinalMappings_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_getOrdinalMapping(ListOf_t* lo, unsigned int n);


/**
 * Get an OrdinalMapping_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the OrdinalMapping_t to
 * retrieve.
 *
 * @return the OrdinalMapping_t in this ListOf_t with the given @p sid or
 * @c NULL if no such OrdinalMapping_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfOrdinalMappings_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth OrdinalMapping_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the OrdinalMapping_t to
 * remove.
 *
 * @return a pointer to the nth OrdinalMapping_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfOrdinalMappings_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the OrdinalMapping_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the OrdinalMapping_t to
 * remove.
 *
 * @return the OrdinalMapping_t in this ListOf_t based on the identifier or
 * NULL if no such OrdinalMapping_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfOrdinalMappings_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfOrdinalMappings_H__ */


