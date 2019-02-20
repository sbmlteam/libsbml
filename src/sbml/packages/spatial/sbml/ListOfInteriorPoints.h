/**
 * @file ListOfInteriorPoints.h
 * @brief Definition of the ListOfInteriorPoints class.
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
 * @class ListOfInteriorPoints
 * @sbmlbrief{spatial} TODO:Definition of the ListOfInteriorPoints class.
 */


#ifndef ListOfInteriorPoints_H__
#define ListOfInteriorPoints_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/InteriorPoint.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfInteriorPoints : public ListOf
{

public:

  /**
   * Creates a new ListOfInteriorPoints using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfInteriorPoints.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfInteriorPoints.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfInteriorPoints.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfInteriorPoints(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                       unsigned int version =
                         SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion =
                         SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfInteriorPoints using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfInteriorPoints(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfInteriorPoints.
   *
   * @param orig the ListOfInteriorPoints instance to copy.
   */
  ListOfInteriorPoints(const ListOfInteriorPoints& orig);


  /**
   * Assignment operator for ListOfInteriorPoints.
   *
   * @param rhs the ListOfInteriorPoints object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfInteriorPoints& operator=(const ListOfInteriorPoints& rhs);


  /**
   * Creates and returns a deep copy of this ListOfInteriorPoints object.
   *
   * @return a (deep) copy of this ListOfInteriorPoints object.
   */
  virtual ListOfInteriorPoints* clone() const;


  /**
   * Destructor for ListOfInteriorPoints.
   */
  virtual ~ListOfInteriorPoints();


  /**
   * Get an InteriorPoint from the ListOfInteriorPoints.
   *
   * @param n an unsigned int representing the index of the InteriorPoint to
   * retrieve.
   *
   * @return the nth InteriorPoint in this ListOfInteriorPoints.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see get(const std::string& sid)
   * @see getNumInteriorPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual InteriorPoint* get(unsigned int n);


  /**
   * Get an InteriorPoint from the ListOfInteriorPoints.
   *
   * @param n an unsigned int representing the index of the InteriorPoint to
   * retrieve.
   *
   * @return the nth InteriorPoint in this ListOfInteriorPoints.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see get(const std::string& sid)
   * @see getNumInteriorPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const InteriorPoint* get(unsigned int n) const;


  /**
   * Get an InteriorPoint from the ListOfInteriorPoints based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the InteriorPoint to
   * retrieve.
   *
   * @return the InteriorPoint in this ListOfInteriorPoints with the given @p
   * sid or @c NULL if no such InteriorPoint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see get(unsigned int n)
   * @see getNumInteriorPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual InteriorPoint* get(const std::string& sid);


  /**
   * Get an InteriorPoint from the ListOfInteriorPoints based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the InteriorPoint to
   * retrieve.
   *
   * @return the InteriorPoint in this ListOfInteriorPoints with the given @p
   * sid or @c NULL if no such InteriorPoint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see get(unsigned int n)
   * @see getNumInteriorPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const InteriorPoint* get(const std::string& sid) const;


  /**
   * Removes the nth InteriorPoint from this ListOfInteriorPoints and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the InteriorPoint to
   * remove.
   *
   * @return a pointer to the nth InteriorPoint in this ListOfInteriorPoints.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumInteriorPoints()
   * @see remove(const std::string& sid)
   */
  virtual InteriorPoint* remove(unsigned int n);


  /**
   * Removes the InteriorPoint from this ListOfInteriorPoints based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the InteriorPoint to
   * remove.
   *
   * @return the InteriorPoint in this ListOfInteriorPoints based on the
   * identifier or NULL if no such InteriorPoint exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumInteriorPoints()
   * @see remove(unsigned int n)
   */
  virtual InteriorPoint* remove(const std::string& sid);


  /**
   * Adds a copy of the given InteriorPoint to this ListOfInteriorPoints.
   *
   * @param ip the InteriorPoint object to add.
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
   * @see createInteriorPoint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumInteriorPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addInteriorPoint(const InteriorPoint* ip);


  /**
   * Get the number of InteriorPoint objects in this ListOfInteriorPoints.
   *
   * @return the number of InteriorPoint objects in this ListOfInteriorPoints.
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumInteriorPoints() const;


  /**
   * Creates a new InteriorPoint object, adds it to this ListOfInteriorPoints
   * object and returns the InteriorPoint object created.
   *
   * @return a new InteriorPoint object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumInteriorPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  InteriorPoint* createInteriorPoint();


  /**
   * Returns the XML element name of this ListOfInteriorPoints object.
   *
   * For ListOfInteriorPoints, the XML element name is always
   * @c "listOfInteriorPoints".
   *
   * @return the name of this element, i.e. @c "listOfInteriorPoints".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfInteriorPoints object.
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
   * ListOfInteriorPoints object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfInteriorPoints:
   * @sbmlconstant{SBML_SPATIAL_INTERIORPOINT, SBMLSpatialTypeCode_t}.
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
   * Creates a new InteriorPoint in this ListOfInteriorPoints
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
 * Get an InteriorPoint_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the InteriorPoint_t to
 * retrieve.
 *
 * @return the nth InteriorPoint_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfInteriorPoints_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_getInteriorPoint(ListOf_t* lo, unsigned int n);


/**
 * Get an InteriorPoint_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the InteriorPoint_t to
 * retrieve.
 *
 * @return the InteriorPoint_t in this ListOf_t with the given @p sid or
 * @c NULL if no such InteriorPoint_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfInteriorPoints_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth InteriorPoint_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the InteriorPoint_t to
 * remove.
 *
 * @return a pointer to the nth InteriorPoint_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfInteriorPoints_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the InteriorPoint_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the InteriorPoint_t to
 * remove.
 *
 * @return the InteriorPoint_t in this ListOf_t based on the identifier or NULL
 * if no such InteriorPoint_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfInteriorPoints_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfInteriorPoints_H__ */


