/**
 * @file    ListOfExternalModelDefinitions.h
 * @brief   Definition of ListOfExternalModelDefinitions, the SBase derived class of externalModelDefinitions package.
 * @author  Lucian Smith 
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2011 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class ListOfExternalModelDefinitions
 * @sbmlpackage comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %ListOfExternalModelDefinitions construct
 * from the &ldquo;comp&rdquo; package.
 * 
 * The ListOfExternalModelDefinitions is a container for the extended 
 * SBMLDocument that defines references to Models defined in external
 * files.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see ExternalModelDefinition
 * @see ListOfDeletions
 * @see ListOfModelDefinitions
 * @see ListOfPorts
 * @see ListOfReplacedElements
 * @see ListOfSubmodels
 */


#ifndef ListOfExternalModelDefinitions_H__
#define ListOfExternalModelDefinitions_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/ListOf.h>
#include <sbml/packages/comp/sbml/ExternalModelDefinition.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfExternalModelDefinitions : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfExternalModelDefinitions object.
   *
   * @return a (deep) copy of this ListOfExternalModelDefinitions.
   */
  virtual ListOfExternalModelDefinitions* clone () const;


  /**
   * Creates a new ListOfExternalModelDefinitions with the given level,
   * version, and package version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  ListOfExternalModelDefinitions(unsigned int level      = CompExtension::getDefaultLevel(), 
                                 unsigned int version    = CompExtension::getDefaultVersion(), 
                                 unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfExternalModelDefinitions with the given
   * CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  ListOfExternalModelDefinitions(CompPkgNamespaces* compns);


  /**
   * Get a ExternalModelDefinition from the ListOfExternalModelDefinitions.
   *
   * @param n the index number of the ExternalModelDefinition to get.
   * 
   * @return the nth ExternalModelDefinition in this
   * ListOfExternalModelDefinitions.
   *
   * @see size()
   */
  virtual ExternalModelDefinition* get(unsigned int n); 


  /**
   * Get a ExternalModelDefinition from the ListOfExternalModelDefinitions.
   *
   * @param n the index number of the ExternalModelDefinition to get.
   * 
   * @return the nth ExternalModelDefinition in this
   * ListOfExternalModelDefinitions.
   *
   * @see size()
   */
  virtual const ExternalModelDefinition* get(unsigned int n) const; 


  /**
   * Get a Model from the ListOfExternalModelDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Model to get.
   * 
   * @return Model in this ListOfExternalModelDefinitions
   * with the given @p sid or @c NULL if no such
   * Member exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual ExternalModelDefinition* get (const std::string& sid);


  /**
   * Get a Model from the ListOfExternalModelDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Model to get.
   * 
   * @return Model in this ListOfExternalModelDefinitions
   * with the given @p sid or @c NULL if no such
   * Model exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const ExternalModelDefinition* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfExternalModelDefinitions items and
   * returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   *
   * @see size()
   */
  virtual ExternalModelDefinition* remove (unsigned int n);


  /**
   * Removes the item with given @p sid from this ListOfModelDefinitions items
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param sid the id of the item to remove
   *
   * @see size()
   */  
   virtual ExternalModelDefinition* remove (const std::string &sid);


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., Model objects, if the list is non-empty).
   * 
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for objects contained in this list:
   * @link SBMLTypeCode_t#SBML_COMP_EXTERNALMODELDEFINITION SBML_COMP_EXTERNALMODELDEFINITION@endlink (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Accepts the given SBMLVisitor.
   *
   * @param v the SBMLVisitor instance to be used.
   * 
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next item in the
   * list.
   */

  virtual bool accept(SBMLVisitor& v) const;
  /** @endcond */


protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * Create and return an SBML object of this class, if present.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  virtual void writeXMLNS (XMLOutputStream& stream) const;
  /** @endcond */
};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ListOfExternalModelDefinitions_H__ */
