/**
 * @file ListOfDistribInputs.h
 * @brief Definition of the ListOfDistribInputs class.
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
 * @class ListOfDistribInputs
 * @sbmlbrief{distrib} TODO:Definition of the ListOfDistribInputs class.
 */


#ifndef ListOfDistribInputs_H__
#define ListOfDistribInputs_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribListOfBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribInput.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfDistribInputs : public DistribListOfBase
{

public:

  /**
   * Creates a new ListOfDistribInputs using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfDistribInputs.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfDistribInputs.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this ListOfDistribInputs.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDistribInputs(unsigned int level = DistribExtension::getDefaultLevel(),
                      unsigned int version =
                        DistribExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDistribInputs using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDistribInputs(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for ListOfDistribInputs.
   *
   * @param orig the ListOfDistribInputs instance to copy.
   */
  ListOfDistribInputs(const ListOfDistribInputs& orig);


  /**
   * Assignment operator for ListOfDistribInputs.
   *
   * @param rhs the ListOfDistribInputs object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfDistribInputs& operator=(const ListOfDistribInputs& rhs);


  /**
   * Creates and returns a deep copy of this ListOfDistribInputs object.
   *
   * @return a (deep) copy of this ListOfDistribInputs object.
   */
  virtual ListOfDistribInputs* clone() const;


  /**
   * Destructor for ListOfDistribInputs.
   */
  virtual ~ListOfDistribInputs();


  /**
   * Get a DistribInput from the ListOfDistribInputs.
   *
   * @param n an unsigned int representing the index of the DistribInput to
   * retrieve.
   *
   * @return the nth DistribInput in this ListOfDistribInputs.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see get(const std::string& sid)
   * @see getNumDistribInputs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DistribInput* get(unsigned int n);


  /**
   * Get a DistribInput from the ListOfDistribInputs.
   *
   * @param n an unsigned int representing the index of the DistribInput to
   * retrieve.
   *
   * @return the nth DistribInput in this ListOfDistribInputs.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see get(const std::string& sid)
   * @see getNumDistribInputs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DistribInput* get(unsigned int n) const;


  /**
   * Get a DistribInput from the ListOfDistribInputs based on its identifier.
   *
   * @param sid a string representing the identifier of the DistribInput to
   * retrieve.
   *
   * @return the DistribInput in this ListOfDistribInputs with the given @p sid
   * or @c NULL if no such DistribInput exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see get(unsigned int n)
   * @see getNumDistribInputs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DistribInput* get(const std::string& sid);


  /**
   * Get a DistribInput from the ListOfDistribInputs based on its identifier.
   *
   * @param sid a string representing the identifier of the DistribInput to
   * retrieve.
   *
   * @return the DistribInput in this ListOfDistribInputs with the given @p sid
   * or @c NULL if no such DistribInput exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see get(unsigned int n)
   * @see getNumDistribInputs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DistribInput* get(const std::string& sid) const;


  /**
   * Get a DistribInput from the ListOfDistribInputs based on its 'index' attribute.
   *
   * @param n the value of the index attribute of the DistribInput to get.
   *
   * @return the DistribInput in this ListOfDistribInputs with the given index attribute, or NULL if no such element exists.
   *
   * @see get(unsigned int n)   *
   * @see get(const std::string& sid)   *
   */
  virtual DistribInput* getByIndex(unsigned int n);


  /**
   * Get a DistribInput from the ListOfDistribInputs based on its 'index' attribute.
   *
   * @param n the value of the index attribute of the DistribInput to get.
   *
   * @return the DistribInput in this ListOfDistribInputs with the given index attribute, or NULL if no such element exists.
   *
   * @see get(unsigned int n)   *
   * @see get(const std::string& sid)   *
   */
  virtual const DistribInput* getByIndex(unsigned int n) const;


  /**
   * Removes the nth DistribInput from this ListOfDistribInputs and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the DistribInput to
   * remove.
   *
   * @return a pointer to the nth DistribInput in this ListOfDistribInputs.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribInputs()
   * @see remove(const std::string& sid)
   */
  virtual DistribInput* remove(unsigned int n);


  /**
   * Removes the DistribInput from this ListOfDistribInputs based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the DistribInput to
   * remove.
   *
   * @return the DistribInput in this ListOfDistribInputs based on the
   * identifier or NULL if no such DistribInput exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribInputs()
   * @see remove(unsigned int n)
   */
  virtual DistribInput* remove(const std::string& sid);


  /**
   * Adds a copy of the given DistribInput to this ListOfDistribInputs.
   *
   * @param di the DistribInput object to add.
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
   * @see createDistribInput()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribInputs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addDistribInput(const DistribInput* di);


  /**
   * Get the number of DistribInput objects in this ListOfDistribInputs.
   *
   * @return the number of DistribInput objects in this ListOfDistribInputs.
   *
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumDistribInputs() const;


  /**
   * Creates a new DistribInput object, adds it to this ListOfDistribInputs
   * object and returns the DistribInput object created.
   *
   * @return a new DistribInput object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribInputs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  DistribInput* createDistribInput();


  /**
   * Returns the XML element name of this ListOfDistribInputs object.
   *
   * For ListOfDistribInputs, the XML element name is always
   * @c "listOfInputs".
   *
   * @return the name of this element, i.e. @c "listOfInputs".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfDistribInputs object.
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
   * ListOfDistribInputs object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfDistribInputs:
   * @sbmlconstant{SBML_DISTRIB_DISTRIBINPUT, SBMLDistribTypeCode_t}.
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
   * Creates a new DistribInput in this ListOfDistribInputs
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Distrib package
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
 * Get a DistribInput_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribInput_t to
 * retrieve.
 *
 * @return the nth DistribInput_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDistribInputs_t
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_getDistribInput(ListOf_t* lo, unsigned int n);


/**
 * Get a DistribInput_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribInput_t to
 * retrieve.
 *
 * @return the DistribInput_t in this ListOf_t with the given @p sid or @c NULL
 * if no such DistribInput_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDistribInputs_t
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth DistribInput_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribInput_t to
 * remove.
 *
 * @return a pointer to the nth DistribInput_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfDistribInputs_t
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the DistribInput_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribInput_t to
 * remove.
 *
 * @return the DistribInput_t in this ListOf_t based on the identifier or NULL
 * if no such DistribInput_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfDistribInputs_t
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfDistribInputs_H__ */


