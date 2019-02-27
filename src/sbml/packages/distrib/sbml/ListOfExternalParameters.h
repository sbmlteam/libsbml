/**
 * @file ListOfExternalParameters.h
 * @brief Definition of the ListOfExternalParameters class.
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


#include <sbml/ListOf.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/ExternalParameter.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfExternalParameters : public ListOf
{
protected:

  /** @cond doxygenLibsbmlInternal */


  /** @endcond */

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
   * Returns the value of the "id" attribute of this ListOfExternalParameters.
   *
   * @return the value of the "id" attribute of this ListOfExternalParameters
   * as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * ListOfExternalParameters.
   *
   * @return the value of the "name" attribute of this ListOfExternalParameters
   * as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this ListOfExternalParameters's "id"
   * attribute is set.
   *
   * @return @c true if this ListOfExternalParameters's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this ListOfExternalParameters's "name"
   * attribute is set.
   *
   * @return @c true if this ListOfExternalParameters's "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this ListOfExternalParameters.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * Calling this function with @p id = @c NULL or an empty string is
   * equivalent to calling unsetId().
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "name" attribute of this ListOfExternalParameters.
   *
   * @param name std::string& value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p name = @c NULL or an empty string is
   * equivalent to calling unsetName().
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "id" attribute of this ListOfExternalParameters.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this ListOfExternalParameters.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Get an ExternalParameter from the ListOfExternalParameters.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to retrieve.
   *
   * @return the nth ExternalParameter in this ListOfExternalParameters.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see get(const std::string& sid)
   * @see getNumExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual ExternalParameter* get(unsigned int n);


  /**
   * Get an ExternalParameter from the ListOfExternalParameters.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to retrieve.
   *
   * @return the nth ExternalParameter in this ListOfExternalParameters.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see get(const std::string& sid)
   * @see getNumExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const ExternalParameter* get(unsigned int n) const;


  /**
   * Get an ExternalParameter from the ListOfExternalParameters based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ExternalParameter
   * to retrieve.
   *
   * @return the ExternalParameter in this ListOfExternalParameters with the
   * given @p sid or @c NULL if no such ExternalParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see get(unsigned int n)
   * @see getNumExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual ExternalParameter* get(const std::string& sid);


  /**
   * Get an ExternalParameter from the ListOfExternalParameters based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ExternalParameter
   * to retrieve.
   *
   * @return the ExternalParameter in this ListOfExternalParameters with the
   * given @p sid or @c NULL if no such ExternalParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see get(unsigned int n)
   * @see getNumExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const ExternalParameter* get(const std::string& sid) const;


  /**
   * Removes the nth ExternalParameter from this ListOfExternalParameters and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to remove.
   *
   * @return a pointer to the nth ExternalParameter in this
   * ListOfExternalParameters.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumExternalParameters()
   * @see remove(const std::string& sid)
   */
  virtual ExternalParameter* remove(unsigned int n);


  /**
   * Removes the ExternalParameter from this ListOfExternalParameters based on
   * its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the ExternalParameter
   * to remove.
   *
   * @return the ExternalParameter in this ListOfExternalParameters based on
   * the identifier or NULL if no such ExternalParameter exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumExternalParameters()
   * @see remove(unsigned int n)
   */
  virtual ExternalParameter* remove(const std::string& sid);


  /**
   * Adds a copy of the given ExternalParameter to this
   * ListOfExternalParameters.
   *
   * @param ep the ExternalParameter object to add.
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
   * @see createExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addExternalParameter(const ExternalParameter* ep);


  /**
   * Get the number of ExternalParameter objects in this
   * ListOfExternalParameters.
   *
   * @return the number of ExternalParameter objects in this
   * ListOfExternalParameters.
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumExternalParameters() const;


  /**
   * Creates a new ExternalParameter object, adds it to this
   * ListOfExternalParameters object and returns the ExternalParameter object
   * created.
   *
   * @return a new ExternalParameter object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumExternalParameters()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  ExternalParameter* createExternalParameter();


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


  /**
   * Predicate returning @c true if all the required attributes for this
   * ListOfExternalParameters object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * ListOfExternalParameters have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;




  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new ExternalParameter in this ListOfExternalParameters
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

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
 * Returns the value of the "id" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this ListOf_t as a pointer to a
 * string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
char *
ListOfExternalParameters_getId(const ListOf_t * lo);


/**
 * Returns the value of the "name" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this ListOf_t as a pointer to a
 * string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
char *
ListOfExternalParameters_getName(const ListOf_t * lo);


/**
 * Predicate returning @c 1 (true) if this ListOf_t's "id" attribute is set.
 *
 * @param lo the ListOf_t structure.
 *
 * @return @c 1 (true) if this ListOf_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_isSetId(const ListOf_t * lo);


/**
 * Predicate returning @c 1 (true) if this ListOf_t's "name" attribute is set.
 *
 * @param lo the ListOf_t structure.
 *
 * @return @c 1 (true) if this ListOf_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_isSetName(const ListOf_t * lo);


/**
 * Sets the value of the "id" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling ListOfExternalParameters_unsetId().
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_setId(ListOf_t * lo, const char * id);


/**
 * Sets the value of the "name" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling ListOfExternalParameters_unsetName().
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_setName(ListOf_t * lo, const char * name);


/**
 * Unsets the value of the "id" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_unsetId(ListOf_t * lo);


/**
 * Unsets the value of the "name" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_unsetName(ListOf_t * lo);


/**
 * Get an ExternalParameter_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ExternalParameter_t
 * to retrieve.
 *
 * @return the nth ExternalParameter_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_getExternalParameter(ListOf_t* lo, unsigned int n);


/**
 * Get an ExternalParameter_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the ExternalParameter_t
 * to retrieve.
 *
 * @return the ExternalParameter_t in this ListOf_t with the given @p sid or
 * @c NULL if no such ExternalParameter_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth ExternalParameter_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ExternalParameter_t
 * to remove.
 *
 * @return a pointer to the nth ExternalParameter_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the ExternalParameter_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the ExternalParameter_t
 * to remove.
 *
 * @return the ExternalParameter_t in this ListOf_t based on the identifier or
 * NULL if no such ExternalParameter_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfExternalParameters_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfExternalParameters_H__ */


