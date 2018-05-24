/**
 * @file ListOfExternalParameters.h
 * @brief Definition of the ListOfExternalParameters class.
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
 * @class ListOfExternalParameters
 * @sbmlbrief{distrib} TODO:Definition of the ListOfExternalParameters class.
 */


#ifndef ListOfExternalParameters_H__
#define ListOfExternalParameters_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribListOfBase.h>
#include <sbml/packages/distrib/sbml/DistribListOfBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribExternalParameter.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfExternalParameters : public DistribListOfBase
{

public:

  /**
   * Creates a new ListOfExternalParameters using the given SBML Level, Version
   * and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfExternalParameters.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfExternalParameters.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this ListOfExternalParameters.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfExternalParameters(
                           unsigned int level =
                             DistribExtension::getDefaultLevel(),
                           unsigned int version =
                             DistribExtension::getDefaultVersion(),
                           unsigned int pkgVersion =
                             DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfExternalParameters using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfExternalParameters(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for ListOfExternalParameters.
   *
   * @param orig the ListOfExternalParameters instance to copy.
   */
  ListOfExternalParameters(const ListOfExternalParameters& orig);


  /**
   * Assignment operator for ListOfExternalParameters.
   *
   * @param rhs the ListOfExternalParameters object whose values are to be used
   * as the basis of the assignment.
   */
  ListOfExternalParameters& operator=(const ListOfExternalParameters& rhs);


  /**
   * Creates and returns a deep copy of this ListOfExternalParameters object.
   *
   * @return a (deep) copy of this ListOfExternalParameters object.
   */
  virtual ListOfExternalParameters* clone() const;


  /**
   * Destructor for ListOfExternalParameters.
   */
  virtual ~ListOfExternalParameters();


  /**
   * Get a DistribExternalParameter from the ListOfExternalParameters.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in this ListOfExternalParameters.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see get(const std::string& sid)
   * @see getNumDistribExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DistribExternalParameter* get(unsigned int n);


  /**
   * Get a DistribExternalParameter from the ListOfExternalParameters.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in this ListOfExternalParameters.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see get(const std::string& sid)
   * @see getNumDistribExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DistribExternalParameter* get(unsigned int n) const;


  /**
   * Get a DistribExternalParameter from the ListOfExternalParameters based on
   * its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in this ListOfExternalParameters with
   * the given @p sid or @c NULL if no such DistribExternalParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see get(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DistribExternalParameter* get(const std::string& sid);


  /**
   * Get a DistribExternalParameter from the ListOfExternalParameters based on
   * its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in this ListOfExternalParameters with
   * the given @p sid or @c NULL if no such DistribExternalParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see get(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DistribExternalParameter* get(const std::string& sid) const;


  /**
   * Removes the nth DistribExternalParameter from this
   * ListOfExternalParameters and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to remove.
   *
   * @return a pointer to the nth DistribExternalParameter in this
   * ListOfExternalParameters.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see remove(const std::string& sid)
   */
  virtual DistribExternalParameter* remove(unsigned int n);


  /**
   * Removes the DistribExternalParameter from this ListOfExternalParameters
   * based on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to remove.
   *
   * @return the DistribExternalParameter in this ListOfExternalParameters
   * based on the identifier or NULL if no such DistribExternalParameter
   * exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see remove(unsigned int n)
   */
  virtual DistribExternalParameter* remove(const std::string& sid);


  /**
   * Adds a copy of the given DistribExternalParameter to this
   * ListOfExternalParameters.
   *
   * @param dep the DistribExternalParameter object to add.
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
   * @see createDistribExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addDistribExternalParameter(const DistribExternalParameter* dep);


  /**
   * Get the number of DistribExternalParameter objects in this
   * ListOfExternalParameters.
   *
   * @return the number of DistribExternalParameter objects in this
   * ListOfExternalParameters.
   *
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumDistribExternalParameters() const;


  /**
   * Creates a new DistribExternalParameter object, adds it to this
   * ListOfExternalParameters object and returns the DistribExternalParameter
   * object created.
   *
   * @return a new DistribExternalParameter object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  DistribExternalParameter* createDistribExternalParameter();


  /**
   * Returns the XML element name of this ListOfExternalParameters object.
   *
   * For ListOfExternalParameters, the XML element name is always
   * @c "listOfExternalParameters".
   *
   * @return the name of this element, i.e. @c "listOfExternalParameters".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfExternalParameters object.
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
   * ListOfExternalParameters object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfExternalParameters:
   * @sbmlconstant{SBML_DISTRIB_EXTERNALPARAMETER, SBMLDistribTypeCode_t}.
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
   * Creates a new DistribExternalParameter in this ListOfExternalParameters
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
 * Get a DistribExternalParameter_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the nth DistribExternalParameter_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_getDistribExternalParameter(ListOf_t* lo,
                                                     unsigned int n);


/**
 * Get a DistribExternalParameter_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the DistribExternalParameter_t in this ListOf_t with the given @p
 * sid or @c NULL if no such DistribExternalParameter_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth DistribExternalParameter_t from this ListOf_t and returns a
 * pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to remove.
 *
 * @return a pointer to the nth DistribExternalParameter_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the DistribExternalParameter_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to remove.
 *
 * @return the DistribExternalParameter_t in this ListOf_t based on the
 * identifier or NULL if no such DistribExternalParameter_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfExternalParameters_H__ */


