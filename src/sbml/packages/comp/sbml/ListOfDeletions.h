/**
 * @file    ListOfDeletions.h
 * @brief   Definition of ListOfDeletions, the SBase derived class of deletions package.
 * @author  Lucian Smith 
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class ListOfDeletions
 * @sbmlbrief{comp} A list of Deletion objects.
 * 
 * The ListOfDeletions is a container for the &ldquo;comp&rdquo;
 * Submodel that defines elements to be removed before instantiation.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see Deletion
 * @see ListOfExternalModelDefinitions
 * @see ListOfModelDefinitions
 * @see ListOfPorts
 * @see ListOfReplacedElements
 * @see ListOfSubmodels
 */


#ifndef ListOfDeletions_H__
#define ListOfDeletions_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/ListOf.h>
#include <sbml/packages/comp/sbml/Deletion.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfDeletions : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfDeletions object.
   *
   * @return a (deep) copy of this ListOfDeletions.
   */
  virtual ListOfDeletions* clone () const;


  /**
   * Creates a new ListOfDeletions with the given level, version, and package
   * version.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDeletions(unsigned int level      = CompExtension::getDefaultLevel(), 
                  unsigned int version    = CompExtension::getDefaultVersion(), 
                  unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDeletions with the given CompPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param compns the CompPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDeletions(CompPkgNamespaces* compns);


  /**
   * Get a Deletion from the ListOfDeletions.
   *
   * @param n the index number of the Deletion to get.
   * 
   * @return the nth Deletion in this ListOfDeletions.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual Deletion* get(unsigned int n); 


  /**
   * Get a Deletion from the ListOfDeletions.
   *
   * @param n the index number of the Deletion to get.
   * 
   * @return the nth Deletion in this ListOfDeletions.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual const Deletion * get(unsigned int n) const; 


  /**
   * Get a Deletion from the ListOfDeletions
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Deletion to get.
   * 
   * @return Deletion in this ListOfDeletions
   * with the given @p sid or @c NULL if no such
   * Member exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual Deletion* get (const std::string& sid);


  /**
   * Get a Deletion from the ListOfDeletions
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Deletion to get.
   * 
   * @return Deletion in this ListOfDeletions
   * with the given @p sid or @c NULL if no such
   * Deletion exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const Deletion* get (const std::string& sid) const;

  
  /**
   * Removes the nth item from this ListOfDeletions items and returns a
   * pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove.
   *
   * @see size()
   */
  virtual Deletion* remove (unsigned int n);


  /**
   * Removes an item from this ListOfDeletions items based on its identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param sid string representing the id of the item to remove.
   *
   * @see size()
   */
  virtual Deletion* remove (const std::string& sid);


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., Deletion objects, if the list is non-empty).
   * 
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for objects contained in this list:
   * @sbmlconstant{SBML_COMP_DELETION, SBMLTypeCode_t} (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


  /**
   * Returns the XML element name of this SBML object.
   *
   * @return the name of this element, as a text string.
   */
  virtual const std::string& getElementName () const;


protected:

  /** @cond doxygenLibsbmlInternal */
  /**
   * Create and return an SBML object of this class, if present.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  virtual void writeXMLNS (XMLOutputStream& stream) const;
  /** @endcond */
};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * Get a Deletion_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Deletion_t to
 * retrieve.
 *
 * @return the nth Deletion_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDeletions_t
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_getDeletion(ListOf_t* lo, unsigned int n);


/**
 * Get a Deletion_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Deletion_t to
 * retrieve.
 *
 * @return the Deletion_t in this ListOf_t with the given @p sid or @c NULL if
 * no such Deletion_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDeletions_t
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth Deletion_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Deletion_t to remove.
 *
 * @return a pointer to the nth Deletion_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfDeletions_t
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the Deletion_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Deletion_t to remove.
 *
 * @return the Deletion_t in this ListOf_t based on the identifier or NULL if
 * no such Deletion_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfDeletions_t
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ListOfDeletions_H__ */
