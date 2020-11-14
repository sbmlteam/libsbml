/**
 * @file ListOfUncertainties.h
 * @brief Definition of the ListOfUncertainties class.
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
 * @class ListOfUncertainties
 * @sbmlbrief{distrib} TODO:Definition of the ListOfUncertainties class.
 */


#ifndef ListOfUncertainties_H__
#define ListOfUncertainties_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/Uncertainty.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfUncertainties : public ListOf
{

public:

  /**
   * Creates a new ListOfUncertainties using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfUncertainties.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfUncertainties.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this ListOfUncertainties.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUncertainties(unsigned int level = DistribExtension::getDefaultLevel(),
                      unsigned int version =
                        DistribExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfUncertainties using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUncertainties(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for ListOfUncertainties.
   *
   * @param orig the ListOfUncertainties instance to copy.
   */
  ListOfUncertainties(const ListOfUncertainties& orig);


  /**
   * Assignment operator for ListOfUncertainties.
   *
   * @param rhs the ListOfUncertainties object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfUncertainties& operator=(const ListOfUncertainties& rhs);


  /**
   * Creates and returns a deep copy of this ListOfUncertainties object.
   *
   * @return a (deep) copy of this ListOfUncertainties object.
   */
  virtual ListOfUncertainties* clone() const;


  /**
   * Destructor for ListOfUncertainties.
   */
  virtual ~ListOfUncertainties();


  /**
   * Get an Uncertainty from the ListOfUncertainties.
   *
   * @param n an unsigned int representing the index of the Uncertainty to
   * retrieve.
   *
   * @return the nth Uncertainty in this ListOfUncertainties or @c NULL if no
   * such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see get(const std::string& sid)
   * @see getNumUncertainties()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual Uncertainty* get(unsigned int n);


  /**
   * Get an Uncertainty from the ListOfUncertainties.
   *
   * @param n an unsigned int representing the index of the Uncertainty to
   * retrieve.
   *
   * @return the nth Uncertainty in this ListOfUncertainties or @c NULL if no
   * such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see get(const std::string& sid)
   * @see getNumUncertainties()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const Uncertainty* get(unsigned int n) const;


  /**
   * Get an Uncertainty from the ListOfUncertainties based on its identifier.
   *
   * @param sid a string representing the identifier of the Uncertainty to
   * retrieve.
   *
   * @return the Uncertainty in this ListOfUncertainties with the given @p sid
   * or @c NULL if no such Uncertainty exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see get(unsigned int n)
   * @see getNumUncertainties()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual Uncertainty* get(const std::string& sid);


  /**
   * Get an Uncertainty from the ListOfUncertainties based on its identifier.
   *
   * @param sid a string representing the identifier of the Uncertainty to
   * retrieve.
   *
   * @return the Uncertainty in this ListOfUncertainties with the given @p sid
   * or @c NULL if no such Uncertainty exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see get(unsigned int n)
   * @see getNumUncertainties()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const Uncertainty* get(const std::string& sid) const;


  /**
   * Removes the nth Uncertainty from this ListOfUncertainties and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the Uncertainty to
   * remove.
   *
   * @return a pointer to the nth Uncertainty in this ListOfUncertainties.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertainties()
   * @see remove(const std::string& sid)
   */
  virtual Uncertainty* remove(unsigned int n);


  /**
   * Removes the Uncertainty from this ListOfUncertainties based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the Uncertainty to
   * remove.
   *
   * @return the Uncertainty in this ListOfUncertainties based on the
   * identifier or NULL if no such Uncertainty exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertainties()
   * @see remove(unsigned int n)
   */
  virtual Uncertainty* remove(const std::string& sid);


  /**
   * Adds a copy of the given Uncertainty to this ListOfUncertainties.
   *
   * @param u the Uncertainty object to add.
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
   * @see createUncertainty()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertainties()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addUncertainty(const Uncertainty* u);


  /**
   * Get the number of Uncertainty objects in this ListOfUncertainties.
   *
   * @return the number of Uncertainty objects in this ListOfUncertainties.
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumUncertainties() const;


  /**
   * Creates a new Uncertainty object, adds it to this ListOfUncertainties
   * object and returns the Uncertainty object created.
   *
   * @return a new Uncertainty object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertainties()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  Uncertainty* createUncertainty();


  /**
   * Returns the XML element name of this ListOfUncertainties object.
   *
   * For ListOfUncertainties, the XML element name is always
   * @c "listOfUncertainties".
   *
   * @return the name of this element, i.e. @c "listOfUncertainties".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfUncertainties object.
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
   * ListOfUncertainties object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfUncertainties:
   * @sbmlconstant{SBML_DISTRIB_UNCERTAINTY, SBMLDistribTypeCode_t}.
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
   * Creates a new Uncertainty in this ListOfUncertainties
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
 * Get an Uncertainty_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Uncertainty_t to
 * retrieve.
 *
 * @return the nth Uncertainty_t in this ListOf_t or @c NULL if no such object
 * exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUncertainties_t
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_getUncertainty(ListOf_t* lo, unsigned int n);


/**
 * Get an Uncertainty_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Uncertainty_t to
 * retrieve.
 *
 * @return the Uncertainty_t in this ListOf_t with the given @p sid or @c NULL
 * if no such Uncertainty_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUncertainties_t
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth Uncertainty_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Uncertainty_t to
 * remove.
 *
 * @return a pointer to the nth Uncertainty_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUncertainties_t
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the Uncertainty_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Uncertainty_t to
 * remove.
 *
 * @return the Uncertainty_t in this ListOf_t based on the identifier or NULL
 * if no such Uncertainty_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUncertainties_t
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfUncertainties_H__ */


