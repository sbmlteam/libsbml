/**
 * @file ListOfGeometryDefinitions.h
 * @brief Definition of the ListOfGeometryDefinitions class.
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
 * @class ListOfGeometryDefinitions
 * @sbmlbrief{spatial} TODO:Definition of the ListOfGeometryDefinitions class.
 */


#ifndef ListOfGeometryDefinitions_H__
#define ListOfGeometryDefinitions_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class AnalyticGeometry;
class SampledFieldGeometry;
class CSGeometry;
class ParametricGeometry;
class MixedGeometry;

class LIBSBML_EXTERN ListOfGeometryDefinitions : public ListOf
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new ListOfGeometryDefinitions using the given SBML Level,
   * Version and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfGeometryDefinitions.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfGeometryDefinitions.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfGeometryDefinitions.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGeometryDefinitions(
                            unsigned int level =
                              SpatialExtension::getDefaultLevel(),
                            unsigned int version =
                              SpatialExtension::getDefaultVersion(),
                            unsigned int pkgVersion =
                              SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGeometryDefinitions using the given
   * SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGeometryDefinitions(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfGeometryDefinitions.
   *
   * @param orig the ListOfGeometryDefinitions instance to copy.
   */
  ListOfGeometryDefinitions(const ListOfGeometryDefinitions& orig);


  /**
   * Assignment operator for ListOfGeometryDefinitions.
   *
   * @param rhs the ListOfGeometryDefinitions object whose values are to be
   * used as the basis of the assignment.
   */
  ListOfGeometryDefinitions& operator=(const ListOfGeometryDefinitions& rhs);


  /**
   * Creates and returns a deep copy of this ListOfGeometryDefinitions object.
   *
   * @return a (deep) copy of this ListOfGeometryDefinitions object.
   */
  virtual ListOfGeometryDefinitions* clone() const;


  /**
   * Destructor for ListOfGeometryDefinitions.
   */
  virtual ~ListOfGeometryDefinitions();


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to retrieve.
   *
   * @return the nth GeometryDefinition in this ListOfGeometryDefinitions.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see get(const std::string& sid)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GeometryDefinition* get(unsigned int n);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to retrieve.
   *
   * @return the nth GeometryDefinition in this ListOfGeometryDefinitions.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see get(const std::string& sid)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GeometryDefinition* get(unsigned int n) const;


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to retrieve.
   *
   * @return the GeometryDefinition in this ListOfGeometryDefinitions with the
   * given @p sid or @c NULL if no such GeometryDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GeometryDefinition* get(const std::string& sid);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to retrieve.
   *
   * @return the GeometryDefinition in this ListOfGeometryDefinitions with the
   * given @p sid or @c NULL if no such GeometryDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GeometryDefinition* get(const std::string& sid) const;


  /**
   * Removes the nth GeometryDefinition from this ListOfGeometryDefinitions and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to remove.
   *
   * @return a pointer to the nth GeometryDefinition in this
   * ListOfGeometryDefinitions.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   */
  virtual GeometryDefinition* remove(unsigned int n);


  /**
   * Removes the GeometryDefinition from this ListOfGeometryDefinitions based
   * on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to remove.
   *
   * @return the GeometryDefinition in this ListOfGeometryDefinitions based on
   * the identifier or NULL if no such GeometryDefinition exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(unsigned int n)
   */
  virtual GeometryDefinition* remove(const std::string& sid);


  /**
   * Adds a copy of the given GeometryDefinition to this
   * ListOfGeometryDefinitions.
   *
   * @param gd the GeometryDefinition object to add.
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
   * @see createGeometryDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addGeometryDefinition(const GeometryDefinition* gd);


  /**
   * Get the number of GeometryDefinition objects in this
   * ListOfGeometryDefinitions.
   *
   * @return the number of GeometryDefinition objects in this
   * ListOfGeometryDefinitions.
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumGeometryDefinitions() const;


  /**
   * Creates a new AnalyticGeometry object, adds it to this
   * ListOfGeometryDefinitions object and returns the AnalyticGeometry object
   * created.
   *
   * @return a new AnalyticGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  AnalyticGeometry* createAnalyticGeometry();


  /**
   * Creates a new SampledFieldGeometry object, adds it to this
   * ListOfGeometryDefinitions object and returns the SampledFieldGeometry
   * object created.
   *
   * @return a new SampledFieldGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  SampledFieldGeometry* createSampledFieldGeometry();


  /**
   * Creates a new CSGeometry object, adds it to this ListOfGeometryDefinitions
   * object and returns the CSGeometry object created.
   *
   * @return a new CSGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGeometry* createCSGeometry();


  /**
   * Creates a new ParametricGeometry object, adds it to this
   * ListOfGeometryDefinitions object and returns the ParametricGeometry object
   * created.
   *
   * @return a new ParametricGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  ParametricGeometry* createParametricGeometry();


  /**
   * Creates a new MixedGeometry object, adds it to this
   * ListOfGeometryDefinitions object and returns the MixedGeometry object
   * created.
   *
   * @return a new MixedGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  MixedGeometry* createMixedGeometry();


  /**
   * Returns the XML element name of this ListOfGeometryDefinitions object.
   *
   * For ListOfGeometryDefinitions, the XML element name is always
   * @c "listOfGeometryDefinitions".
   *
   * @return the name of this element, i.e. @c "listOfGeometryDefinitions".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this ListOfGeometryDefinitions object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this ListOfGeometryDefinitions object.
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
   * ListOfGeometryDefinitions object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfGeometryDefinitions:
   * @sbmlconstant{SBML_SPATIAL_GEOMETRYDEFINITION, SBMLSpatialTypeCode_t}.
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
   * Creates a new GeometryDefinition in this ListOfGeometryDefinitions
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Spatial package
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * checks concrete types
   */
  virtual bool isValidTypeForList(SBase* item);

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Get a GeometryDefinition_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GeometryDefinition_t
 * to retrieve.
 *
 * @return the nth GeometryDefinition_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGeometryDefinitions_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_getGeometryDefinition(ListOf_t* lo, unsigned int n);


/**
 * Get a GeometryDefinition_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GeometryDefinition_t
 * to retrieve.
 *
 * @return the GeometryDefinition_t in this ListOf_t with the given @p sid or
 * @c NULL if no such GeometryDefinition_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGeometryDefinitions_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth GeometryDefinition_t from this ListOf_t and returns a
 * pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GeometryDefinition_t
 * to remove.
 *
 * @return a pointer to the nth GeometryDefinition_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfGeometryDefinitions_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the GeometryDefinition_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GeometryDefinition_t
 * to remove.
 *
 * @return the GeometryDefinition_t in this ListOf_t based on the identifier or
 * NULL if no such GeometryDefinition_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfGeometryDefinitions_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfGeometryDefinitions_H__ */


