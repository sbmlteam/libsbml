/**
 * @file ListOfUncertParameters.h
 * @brief Definition of the ListOfUncertParameters class.
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
 * @class ListOfUncertParameters
 * @sbmlbrief{distrib} TODO:Definition of the ListOfUncertParameters class.
 */


#ifndef ListOfUncertParameters_H__
#define ListOfUncertParameters_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/UncertParameter.h>
#include <sbml/packages/distrib/sbml/UncertSpan.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfUncertParameters : public ListOf
{

public:

  /**
   * Creates a new ListOfUncertParameters using the given SBML Level, Version
   * and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfUncertParameters.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfUncertParameters.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this ListOfUncertParameters.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUncertParameters(
                         unsigned int level =
                           DistribExtension::getDefaultLevel(),
                         unsigned int version =
                           DistribExtension::getDefaultVersion(),
                         unsigned int pkgVersion =
                           DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfUncertParameters using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUncertParameters(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for ListOfUncertParameters.
   *
   * @param orig the ListOfUncertParameters instance to copy.
   */
  ListOfUncertParameters(const ListOfUncertParameters& orig);


  /**
   * Assignment operator for ListOfUncertParameters.
   *
   * @param rhs the ListOfUncertParameters object whose values are to be used
   * as the basis of the assignment.
   */
  ListOfUncertParameters& operator=(const ListOfUncertParameters& rhs);


  /**
   * Creates and returns a deep copy of this ListOfUncertParameters object.
   *
   * @return a (deep) copy of this ListOfUncertParameters object.
   */
  virtual ListOfUncertParameters* clone() const;


  /**
   * Destructor for ListOfUncertParameters.
   */
  virtual ~ListOfUncertParameters();


  /**
   * Get an UncertParameter from the ListOfUncertParameters.
   *
   * @param n an unsigned int representing the index of the UncertParameter to
   * retrieve.
   *
   * @return the nth UncertParameter in this ListOfUncertParameters or @c NULL
   * if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see get(const std::string& sid)
   * @see getNumUncertParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual UncertParameter* get(unsigned int n);


  /**
   * Get an UncertParameter from the ListOfUncertParameters.
   *
   * @param n an unsigned int representing the index of the UncertParameter to
   * retrieve.
   *
   * @return the nth UncertParameter in this ListOfUncertParameters or @c NULL
   * if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see get(const std::string& sid)
   * @see getNumUncertParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const UncertParameter* get(unsigned int n) const;


  /**
   * Get an UncertParameter from the ListOfUncertParameters based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the UncertParameter to
   * retrieve.
   *
   * @return the UncertParameter in this ListOfUncertParameters with the given
   * @p sid or @c NULL if no such UncertParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see get(unsigned int n)
   * @see getNumUncertParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual UncertParameter* get(const std::string& sid);


  /**
   * Get an UncertParameter from the ListOfUncertParameters based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the UncertParameter to
   * retrieve.
   *
   * @return the UncertParameter in this ListOfUncertParameters with the given
   * @p sid or @c NULL if no such UncertParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see get(unsigned int n)
   * @see getNumUncertParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const UncertParameter* get(const std::string& sid) const;


  /**
   * Removes the nth UncertParameter from this ListOfUncertParameters and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the UncertParameter to
   * remove.
   *
   * @return a pointer to the nth UncertParameter in this
   * ListOfUncertParameters.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertParameters()
   * @see remove(const std::string& sid)
   */
  virtual UncertParameter* remove(unsigned int n);


  /**
   * Removes the UncertParameter from this ListOfUncertParameters based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the UncertParameter to
   * remove.
   *
   * @return the UncertParameter in this ListOfUncertParameters based on the
   * identifier or NULL if no such UncertParameter exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertParameters()
   * @see remove(unsigned int n)
   */
  virtual UncertParameter* remove(const std::string& sid);


  /**
   * Adds a copy of the given UncertParameter to this ListOfUncertParameters.
   *
   * @param up the UncertParameter object to add.
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
   * @see createUncertParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addUncertParameter(const UncertParameter* up);

  int addUncertSpan(const UncertSpan* up);

  /**
   * Get the number of UncertParameter objects in this ListOfUncertParameters.
   *
   * @return the number of UncertParameter objects in this
   * ListOfUncertParameters.
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumUncertParameters() const;


  /**
   * Creates a new UncertParameter object, adds it to this
   * ListOfUncertParameters object and returns the UncertParameter object
   * created.
   *
   * @return a new UncertParameter object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUncertParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  UncertParameter* createUncertParameter();

  UncertSpan* createUncertSpan();

  /**
   * Get an UncertParameter from the ListOfUncertParameters based on the
   * element to which it refers.
   *
   * @param sid a string representing the "var" attribute of the
   * UncertParameter object to retrieve.
   *
   * @return the first UncertParameter in this ListOfUncertParameters based on
   * the given var attribute or NULL if no such UncertParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const UncertParameter* getByVar(const std::string& sid) const;

  /**
   * Get an UncertParameter from the ListOfUncertParameters based on
   * its type.
   *
   * @param utype the UncertType representing the "type" attribute of the
   * UncertParameter object to retrieve.
   *
   * @return the first UncertParameter in this ListOfUncertParameters based on
   * the given var attribute or NULL if no such UncertParameter exists.
   *
   * Note that while most types must be unique in any ListOfUncertParameters,
   * the exception is external parameters 
   * (@sbmlconstant{DISTRIB_UNCERTTYPE_EXTERNALPARAMETER, UncerType_t}), 
   * which there many be several of.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const UncertParameter * getByType(UncertType_t utype) const;


  /**
   * Get an UncertParameter from the ListOfUncertParameters based on the
   * element to which it refers.
   *
   * @param sid a string representing the "var" attribute of the
   * UncertParameter object to retrieve.
   *
   * @return the first UncertParameter in this ListOfUncertParameters based on
   * the given var attribute or NULL if no such UncertParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  UncertParameter* getByVar(const std::string& sid);


  /**
   * Get an UncertParameter from the ListOfUncertParameters based on
   * its type.
   *
   * @param utype the UncertType representing the "type" attribute of the
   * UncertParameter object to retrieve.
   *
   * @return the first UncertParameter in this ListOfUncertParameters based on
   * the given var attribute or NULL if no such UncertParameter exists.
   *
   * Note that while most types must be unique in any ListOfUncertParameters,
   * the exception is external parameters 
   * (@sbmlconstant{DISTRIB_UNCERTTYPE_EXTERNALPARAMETER, UncerType_t}), 
   * which there many be several of.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  UncertParameter * getByType(UncertType_t utype);


  /**
   * Returns the XML element name of this ListOfUncertParameters object.
   *
   * For ListOfUncertParameters, the XML element name is always
   * @c "listOfUncertParameters".
   *
   * @return the name of this element, i.e. @c "listOfUncertParameters".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfUncertParameters object.
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
   * ListOfUncertParameters object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfUncertParameters:
   * @sbmlconstant{SBML_DISTRIB_UNCERTPARAMETER, SBMLDistribTypeCode_t}.
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
   * Creates a new UncertParameter in this ListOfUncertParameters
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */


  friend class Uncertainty;

  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Distrib package
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;

  /** @endcond */

  virtual bool isValidTypeForList(SBase * item);

};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Get an UncertParameter_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the UncertParameter_t to
 * retrieve.
 *
 * @return the nth UncertParameter_t in this ListOf_t or @c NULL if no such
 * object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUncertParameters_t
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_getUncertParameter(ListOf_t* lo, unsigned int n);


/**
 * Get an UncertParameter_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the UncertParameter_t to
 * retrieve.
 *
 * @return the UncertParameter_t in this ListOf_t with the given @p sid or
 * @c NULL if no such UncertParameter_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUncertParameters_t
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth UncertParameter_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the UncertParameter_t to
 * remove.
 *
 * @return a pointer to the nth UncertParameter_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUncertParameters_t
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the UncertParameter_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the UncertParameter_t to
 * remove.
 *
 * @return the UncertParameter_t in this ListOf_t based on the identifier or
 * NULL if no such UncertParameter_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUncertParameters_t
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfUncertParameters_H__ */


