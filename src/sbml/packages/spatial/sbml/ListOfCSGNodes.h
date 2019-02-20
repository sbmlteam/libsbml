/**
 * @file ListOfCSGNodes.h
 * @brief Definition of the ListOfCSGNodes class.
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
 * @class ListOfCSGNodes
 * @sbmlbrief{spatial} TODO:Definition of the ListOfCSGNodes class.
 */


#ifndef ListOfCSGNodes_H__
#define ListOfCSGNodes_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class CSGPrimitive;
class CSGTranslation;
class CSGRotation;
class CSGScale;
class CSGHomogeneousTransformation;
class CSGSetOperator;

class LIBSBML_EXTERN ListOfCSGNodes : public ListOf
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new ListOfCSGNodes using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfCSGNodes.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfCSGNodes.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfCSGNodes.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCSGNodes(unsigned int level = SpatialExtension::getDefaultLevel(),
                 unsigned int version = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCSGNodes using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCSGNodes(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfCSGNodes.
   *
   * @param orig the ListOfCSGNodes instance to copy.
   */
  ListOfCSGNodes(const ListOfCSGNodes& orig);


  /**
   * Assignment operator for ListOfCSGNodes.
   *
   * @param rhs the ListOfCSGNodes object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfCSGNodes& operator=(const ListOfCSGNodes& rhs);


  /**
   * Creates and returns a deep copy of this ListOfCSGNodes object.
   *
   * @return a (deep) copy of this ListOfCSGNodes object.
   */
  virtual ListOfCSGNodes* clone() const;


  /**
   * Destructor for ListOfCSGNodes.
   */
  virtual ~ListOfCSGNodes();


  /**
   * Get a CSGNode from the ListOfCSGNodes.
   *
   * @param n an unsigned int representing the index of the CSGNode to
   * retrieve.
   *
   * @return the nth CSGNode in this ListOfCSGNodes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see get(const std::string& sid)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual CSGNode* get(unsigned int n);


  /**
   * Get a CSGNode from the ListOfCSGNodes.
   *
   * @param n an unsigned int representing the index of the CSGNode to
   * retrieve.
   *
   * @return the nth CSGNode in this ListOfCSGNodes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see get(const std::string& sid)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const CSGNode* get(unsigned int n) const;


  /**
   * Get a CSGNode from the ListOfCSGNodes based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGNode to
   * retrieve.
   *
   * @return the CSGNode in this ListOfCSGNodes with the given @p sid or
   * @c NULL if no such CSGNode exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual CSGNode* get(const std::string& sid);


  /**
   * Get a CSGNode from the ListOfCSGNodes based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGNode to
   * retrieve.
   *
   * @return the CSGNode in this ListOfCSGNodes with the given @p sid or
   * @c NULL if no such CSGNode exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const CSGNode* get(const std::string& sid) const;


  /**
   * Removes the nth CSGNode from this ListOfCSGNodes and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the CSGNode to remove.
   *
   * @return a pointer to the nth CSGNode in this ListOfCSGNodes.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   */
  virtual CSGNode* remove(unsigned int n);


  /**
   * Removes the CSGNode from this ListOfCSGNodes based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the CSGNode to remove.
   *
   * @return the CSGNode in this ListOfCSGNodes based on the identifier or NULL
   * if no such CSGNode exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(unsigned int n)
   */
  virtual CSGNode* remove(const std::string& sid);


  /**
   * Adds a copy of the given CSGNode to this ListOfCSGNodes.
   *
   * @param csgn the CSGNode object to add.
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
   * @see createCSGNode()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addCSGNode(const CSGNode* csgn);


  /**
   * Get the number of CSGNode objects in this ListOfCSGNodes.
   *
   * @return the number of CSGNode objects in this ListOfCSGNodes.
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumCSGNodes() const;


  /**
   * Creates a new CSGPrimitive object, adds it to this ListOfCSGNodes object
   * and returns the CSGPrimitive object created.
   *
   * @return a new CSGPrimitive object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGPrimitive* createCSGPrimitive();


  /**
   * Creates a new CSGTranslation object, adds it to this ListOfCSGNodes object
   * and returns the CSGTranslation object created.
   *
   * @return a new CSGTranslation object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGTranslation* createCSGTranslation();


  /**
   * Creates a new CSGRotation object, adds it to this ListOfCSGNodes object
   * and returns the CSGRotation object created.
   *
   * @return a new CSGRotation object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGRotation* createCSGRotation();


  /**
   * Creates a new CSGScale object, adds it to this ListOfCSGNodes object and
   * returns the CSGScale object created.
   *
   * @return a new CSGScale object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGScale* createCSGScale();


  /**
   * Creates a new CSGHomogeneousTransformation object, adds it to this
   * ListOfCSGNodes object and returns the CSGHomogeneousTransformation object
   * created.
   *
   * @return a new CSGHomogeneousTransformation object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGHomogeneousTransformation* createCSGHomogeneousTransformation();


  /**
   * Creates a new CSGSetOperator object, adds it to this ListOfCSGNodes object
   * and returns the CSGSetOperator object created.
   *
   * @return a new CSGSetOperator object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGNodes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGSetOperator* createCSGSetOperator();


  /**
   * Returns the XML element name of this ListOfCSGNodes object.
   *
   * For ListOfCSGNodes, the XML element name is always @c "listOfCSGNodes".
   *
   * @return the name of this element, i.e. @c "listOfCSGNodes".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this ListOfCSGNodes object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this ListOfCSGNodes object.
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
   * ListOfCSGNodes object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfCSGNodes:
   * @sbmlconstant{SBML_SPATIAL_CSGNODE, SBMLSpatialTypeCode_t}.
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
   * Creates a new CSGNode in this ListOfCSGNodes
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
 * Get a CSGNode_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGNode_t to
 * retrieve.
 *
 * @return the nth CSGNode_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCSGNodes_t
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_getCSGNode(ListOf_t* lo, unsigned int n);


/**
 * Get a CSGNode_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGNode_t to
 * retrieve.
 *
 * @return the CSGNode_t in this ListOf_t with the given @p sid or @c NULL if
 * no such CSGNode_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCSGNodes_t
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth CSGNode_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGNode_t to remove.
 *
 * @return a pointer to the nth CSGNode_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCSGNodes_t
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the CSGNode_t from this ListOf_t based on its identifier and returns
 * a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGNode_t to remove.
 *
 * @return the CSGNode_t in this ListOf_t based on the identifier or NULL if no
 * such CSGNode_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCSGNodes_t
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfCSGNodes_H__ */


