/**
 * @file    ListOfReplacedElements.h
 * @brief   Definition of ListOfReplacedElements, the SBase derived class of replacedElements package.
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
 * @class ListOfReplacedElements
 * @sbmlbrief{comp} A list of ReplacedElement objects.
 * 
 * The ListOfReplacedElements is a container for any SBase object.  It 
 * contains ReplacedElement objects, which point to elements the parent
 * SBase object is to replace.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see ReplacedElement
 * @see ListOfDeletions
 * @see ListOfExternalModelDefinitions
 * @see ListOfModelDefinitions
 * @see ListOfPorts
 * @see ListOfSubmodels
 */


#ifndef ListOfReplacedElements_H__
#define ListOfReplacedElements_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/ListOf.h>
#include <sbml/packages/comp/sbml/ReplacedElement.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfReplacedElements : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfReplacedElements object.
   *
   * @return a (deep) copy of this ListOfReplacedElements.
   */
  virtual ListOfReplacedElements* clone () const;


  /**
   * Creates a new ListOfReplacedElements with the given level, version, and
   * package version.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfReplacedElements(unsigned int level      = CompExtension::getDefaultLevel(), 
                         unsigned int version    = CompExtension::getDefaultVersion(), 
                         unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfReplacedElements with the given CompPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param compns the CompPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfReplacedElements(CompPkgNamespaces* compns);


  /**
   * Get a ReplacedElement from the ListOfReplacedElements.
   *
   * @param n the index number of the ReplacedElement to get.
   * 
   * @return the nth ReplacedElement in this ListOfReplacedElements.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual ReplacedElement* get(unsigned int n); 


  /**
   * Get a ReplacedElement from the ListOfReplacedElements.
   *
   * @param n the index number of the ReplacedElement to get.
   * 
   * @return the nth ReplacedElement in this ListOfReplacedElements.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual const ReplacedElement * get(unsigned int n) const; 


  /**
   * Removes the nth item from this ListOfReplacedElements items and returns
   * a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove.
   *
   * @see size()
   */
  virtual ReplacedElement* remove (unsigned int n);


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., ReplacedElements objects, if the list is non-empty).
   * 
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for objects contained in this list:
   * @sbmlconstant{SBML_COMP_REPLACEDELEMENT, SBMLTypeCode_t} (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


  /**
   * Returns the XML element name of
   * this SBML object.
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
 * Get a ReplacedElement_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ReplacedElement_t to
 * retrieve.
 *
 * @return the nth ReplacedElement_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfReplacedElements_t
 */
LIBSBML_EXTERN
ReplacedElement_t*
ListOfReplacedElements_getReplacedElement(ListOf_t* lo, unsigned int n);


/**
 * Removes the nth ReplacedElement_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ReplacedElement_t to
 * remove.
 *
 * @return a pointer to the nth ReplacedElement_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfReplacedElements_t
 */
LIBSBML_EXTERN
ReplacedElement_t*
ListOfReplacedElements_remove(ListOf_t* lo, unsigned int n);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ListOfReplacedElements_H__ */
