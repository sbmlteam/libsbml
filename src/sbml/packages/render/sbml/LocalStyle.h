/**
 * @file    LocalStyle.h
 * @brief Definition of the LocalStyle class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
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
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class LocalStyle
 * @sbmlbrief{render} %Style information object used in LocalRenderInformation.
 *
 * Local styles are the style information objects used in
 * LocalRenderInformation.  Local styles can be associated with layout
 * objects by role and type as well as by id of layout objects from the
 * layout the local style belongs to.
 *
 * Since LocalStyle is derived from Style, it inherits all of the methods
 * and attributes from Style.
 *
 * @see LocalRenderInformation
 * @see Style
 */

#ifndef LocalStyle_H__
#define LocalStyle_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>
#include <sbml/packages/render/sbml/Style.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus

#include <set>
#include <string>




LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN LocalStyle : public Style
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::set<std::string> mIdList;

  /** @endcond */

public:

  /**
   * Creates a new LocalStyle using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this LocalStyle.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * LocalStyle.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this LocalStyle.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LocalStyle(unsigned int level = RenderExtension::getDefaultLevel(),
             unsigned int version = RenderExtension::getDefaultVersion(),
             unsigned int pkgVersion =
               RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new LocalStyle using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LocalStyle(RenderPkgNamespaces *renderns);


  /**
   * Creates a new LocalStyle object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * LocalStyle object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the LocalStyle
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  LocalStyle(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a LocalStyle with an empty group
   * and empty id, role and type list.
   * The group has to be filled before the
   * object is valid.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  LocalStyle(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for LocalStyle.
   *
   * @param orig the LocalStyle instance to copy.
   */
  LocalStyle(const LocalStyle& orig);


  /**
   * Assignment operator for LocalStyle.
   *
   * @param rhs the LocalStyle object whose values are to be used as the basis
   * of the assignment.
   */
  LocalStyle& operator=(const LocalStyle& rhs);


  /**
   * Creates and returns a deep copy of this LocalStyle object.
   *
   * @return a (deep) copy of this LocalStyle object.
   */
  virtual LocalStyle* clone() const;


  /**
   * Destructor for LocalStyle.
   */
  virtual ~LocalStyle();


  /**
   * Returns the value of the "idList" attribute of this LocalStyle.
   *
   * @return the value of the "idList" attribute of this LocalStyle as a
   * string.
   */
  std::set<std::string>& getIdList();

  /**
   * Returns the value of the "idList" attribute of this LocalStyle.
   *
   * @return the value of the "idList" attribute of this LocalStyle as a
   * string.
   */
  const std::set<std::string>& getIdList() const;

  /**
   * Returns the number of ids in the id set.
   *
   * @return the number of ids in the id set
   */
  unsigned int getNumIds() const;


  /**
   * Checks whether a given @p id is in the id list.
   *
   * @param id the id to be searched for
   *
   * @return @c true or @c false depending on whether the given @p id is in the id list or not.
   */
  bool isInIdList(const std::string& id) const;


  /**
   * Adds another id to the set.
   *
   * @param id the id string to be added to the id list.
   */
  int addId(const std::string& id);


  /** 
   * @return the string of all roles
   */
  std::string createIdString() const;


  /**
   * Removes an id from the set.
   *
   * @param id the id to be removed from the id list.
   */
  int removeId(const std::string& id);


  /**
   * Sets the id list.
   *
   * @param idList The list of ids to be set on the local style.
   */
  int setIdList(const std::set<std::string>& idList);

 
  /**
   * Returns the XML element name of this LocalStyle object.
   *
   * For LocalStyle, the XML element name is always @c "style".
   *
   * @return the name of this element, i.e. @c "style".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this LocalStyle object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_LOCALSTYLE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;



  /**
   * Creates an XMLNode object from this LocalStyle object.
   *
   * @return the XMLNode with the XML representation for the 
   * LocalStyle object.
   */
  XMLNode toXML() const;


protected:

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
   * This method adds the attribute for the list of ids to
   * the given XMLnode.
   *
   * @param node the node where the attribute needs to be added
   */
  void addListOfIds(XMLToken& node) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the id list to an XML stream.
   */
  void writeIdList(XMLOutputStream& stream) const;
  /** @endcond */
};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new LocalStyle_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this LocalStyle_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * LocalStyle_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * LocalStyle_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LocalStyle_t
 */
LIBSBML_EXTERN
LocalStyle_t *
LocalStyle_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this LocalStyle_t object.
 *
 * @param ls the LocalStyle_t structure.
 *
 * @return a (deep) copy of this LocalStyle_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LocalStyle_t
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalStyle_clone(const LocalStyle_t* ls);


/**
 * Frees this LocalStyle_t object.
 *
 * @param ls the LocalStyle_t structure.
 *
 * @memberof LocalStyle_t
 */
LIBSBML_EXTERN
void
LocalStyle_free(LocalStyle_t* ls);

// render FIX_ME
///**
// * Returns the value of the "idList" attribute of this LocalStyle_t.
// *
// * @param ls the LocalStyle_t structure whose idList is sought.
// *
// * @return the value of the "idList" attribute of this LocalStyle_t as a
// * pointer to a string.
// *
// * @copydetails doc_warning_returns_owned_char
// *
// * @memberof LocalStyle_t
// */
//LIBSBML_EXTERN
//char *
//LocalStyle_getIdList(const LocalStyle_t * ls);
//
//
/**
 * Predicate returning @c 1 (true) if this LocalStyle_t's "idList" attribute is
 * set.
 *
 * @param ls the LocalStyle_t structure.
 *
 * @return @c 1 (true) if this LocalStyle_t's "idList" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LocalStyle_t
 */
LIBSBML_EXTERN
int
LocalStyle_isSetIdList(const LocalStyle_t * ls);


/**
 * Sets the value of the "idList" attribute of this LocalStyle_t.
 *
 * @param ls the LocalStyle_t structure.
 *
 * @param idList const char * value of the "idList" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p idList = @c NULL or an empty string is
 * equivalent to calling LocalStyle_unsetIdList().
 *
 * @memberof LocalStyle_t
 */
LIBSBML_EXTERN
int
LocalStyle_setIdList(LocalStyle_t * ls, const char * idList);


///**
// * Unsets the value of the "idList" attribute of this LocalStyle_t.
// *
// * @param ls the LocalStyle_t structure.
// *
// * @copydetails doc_returns_success_code
// * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
// * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
// * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
// *
// * @memberof LocalStyle_t
// */
//LIBSBML_EXTERN
//int
//LocalStyle_unsetIdList(LocalStyle_t * ls);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LocalStyle_t object have been set.
 *
 * @param ls the LocalStyle_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * LocalStyle_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof LocalStyle_t
 */
LIBSBML_EXTERN
int
LocalStyle_hasRequiredAttributes(const LocalStyle_t * ls);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !LocalStyle_H__ */


