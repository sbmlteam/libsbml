/**
 * @file   FbcReactionPlugin.h
 * @brief  Implementation of the FbcReactionPlugin class
 * @author SBMLTeam
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
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class FbcReactionPlugin
 * @sbmlbrief{fbc} Extension of Reaction by the &ldquo;fbc&rdquo; package.
 *
 * The FbcReactionPlugin class inherits from the SBasePlugin class, and
 * codifies the extentions to the Reaction class defined in the @ref fbc
 * package (&ldquo;fbc&rdquo;).  This extention allows the modeler to define
 * (in Version&nbsp;2 of the &ldquo;fbc&rdquo; package) an upper and lower
 * flux bound, with the 'upperFluxBound' and 'lowerFluxBound' attributes,
 * as well as a way to reference any GeneProduct associated with
 * this Reaction, through the GeneProductAssociation child.
 *
 * @note In Version&nbsp;1 of &ldquo;fbc&rdquo;, the FluxBound element is
 * used instead of the 'upperFluxBound' and 'lowerFluxBound' attributes.
 * There is no equivalent of the GeneProductAssociation, which was added 
 * in Version&nbsp;2.
 */


#ifndef FbcReactionPlugin_H__
#define FbcReactionPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/fbc/extension/FbcSBasePlugin.h>
#include <sbml/packages/fbc/sbml/GeneProductAssociation.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN FbcReactionPlugin : public FbcSBasePlugin
{
public:

  /**
   * Creates a new FbcReactionPlugin object using the given parameters.
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @copydetails doc_what_are_sbmlnamespaces
   *
   * @param uri the URI of the SBML Level&nbsp;3 package implemented by
   * this libSBML package extension.
   *
   * @param prefix the XML namespace prefix being used for the package.
   *
   * @param fbcns the namespaces object for the package.
   */
  FbcReactionPlugin(const std::string& uri, const std::string& prefix, 
                                 FbcPkgNamespaces* fbcns);


  /**
   * Copy constructor for FbcReactionPlugin.
   *
   * @param orig the FbcReactionPlugin instance to copy.
   */
  FbcReactionPlugin(const FbcReactionPlugin& orig);


   /**
   * Assignment operator for FbcReactionPlugin.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment.
   */
  FbcReactionPlugin& operator=(const FbcReactionPlugin& rhs);


   /**
   * Creates and returns a deep copy of this FbcReactionPlugin object.
   *
   * @return a (deep) copy of this FbcReactionPlugin object.
   */
  virtual FbcReactionPlugin* clone () const;


   /**
   * Destructor for FbcReactionPlugin.
   */
  virtual ~FbcReactionPlugin();


   //---------------------------------------------------------------
  //
  // overridden virtual functions for read/write/check
  //
  //---------------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


  /** @endcond */

  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond */


  //---------------------------------------------------------------


  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond */

  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond */


  //---------------------------------------------------------------
  //
  // Functions for interacting with the members of the plugin
  //
  //---------------------------------------------------------------

  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the GeneProductAssociation from this FbcReactionPlugin object.
   *
   * @copydetails doc_note_geneproduct_v2_only
   *
   * @return the GeneProductAssociation from object in this FbcReactionPlugin object.
   */
  const GeneProductAssociation* getGeneProductAssociation () const;


  /**
   * Returns the GeneProductAssociation from this FbcReactionPlugin object.
   *
   * @copydetails doc_note_geneproduct_v2_only
   *
   * @return the GeneProductAssociation from object in this FbcReactionPlugin object.
   */
  GeneProductAssociation* getGeneProductAssociation ();


  /**
   * Predicate returning @c true if this FbcReactionPlugin's
   * "GeneProductAssociation" element has been set.
   *
   * @copydetails doc_note_geneproduct_v2_only
   *
   * @return @c true if the "GeneProductAssociation" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetGeneProductAssociation() const;


  /**
   * Sets the GeneProductAssociation element in this FbcReactionPlugin object.
   *
   * @copydetails doc_note_geneproduct_v2_only
   *
   * @param geneProductAssociation the geneProductAssociation to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setGeneProductAssociation (const GeneProductAssociation* geneProductAssociation);


  /**
   * Creates a new GeneProductAssociation object and adds it to the FbcReactionPlugin object.
   *
   * @copydetails doc_note_geneproduct_v2_only
   *
   * @return the newly created GeneProductAssociation object.
   */
  GeneProductAssociation* createGeneProductAssociation ();


  /**
   * Returns the value of the "lowerFluxBound" attribute of this FbcReactionPlugin.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @return the value of the "lowerFluxBound" attribute of this FbcReactionPlugin as a string.
   */
  virtual const std::string& getLowerFluxBound() const;


  /**
   * Returns the value of the "upperFluxBound" attribute of this FbcReactionPlugin.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @return the value of the "upperFluxBound" attribute of this FbcReactionPlugin as a string.
   */
  virtual const std::string& getUpperFluxBound() const;


  /**
   * Predicate returning @c true if this FbcReactionPlugin's "lowerFluxBound"
   * attribute is set.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @return @c true if this FbcReactionPlugin's "lowerFluxBound" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetLowerFluxBound() const;


  /**
   * Predicate returning @c true if this FbcReactionPlugin's "upperFluxBound"
   * attribute is set.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @return @c true if this FbcReactionPlugin's "upperFluxBound" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetUpperFluxBound() const;


  /**
   * Sets the value of the "lowerFluxBound" attribute of this FbcReactionPlugin.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @param lowerFluxBound the value of the "lowerFluxBound" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setLowerFluxBound(const std::string& lowerFluxBound);


  /**
   * Sets the value of the "upperFluxBound" attribute of this FbcReactionPlugin.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @param upperFluxBound the value of the "upperFluxBound" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setUpperFluxBound(const std::string& upperFluxBound);


  /**
   * Unsets the value of the "lowerFluxBound" attribute of this FbcReactionPlugin.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetLowerFluxBound();


  /**
   * Unsets the value of the "upperFluxBound" attribute of this FbcReactionPlugin.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetUpperFluxBound();

  /**
   * Unsets the the "geneProduct" element of this FbcReactionPlugin.
   *
   * @copydetails doc_note_fluxbound_v2_only
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetGeneProductAssociation();

  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  virtual void connectToParent (SBase* sbase);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);


  /** @endcond */


  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this FbcReactionPlugin's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this FbcReactionPlugin's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this
   * FbcReactionPlugin.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this FbcReactionPlugin.
   *
   * @param objectName, the name of the element to create.
   *
   * pointer to the object created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this FbcReactionPlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this FbcReactionPlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */
  /** @cond doxygenLibsbmlInternal */

  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  GeneProductAssociation* mGeneProductAssociation;
  std::string   mLowerFluxBound;
  std::string   mUpperFluxBound;

  /** @endcond */


};

LIBSBML_CPP_NAMESPACE_END


#endif  /* __cplusplus */
#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS



/**
 * Creates a new GeneProductAssociation_t object and adds it to the FbcReactionPlugin_t object.
 *
 * @param fbc the FbcReactionPlugin_t that should have the GeneProductAssociation_t added
 *
 * @copydetails doc_note_geneproduct_v2_only
 *
 * @return pointer to the newly created GeneProductAssociation_t object.
 */
LIBSBML_EXTERN
GeneProductAssociation_t*
FbcReactionPlugin_createGeneProductAssociation(FbcSBasePlugin_t * fbc);


/**
 * Takes a FbcReactionPlugin_t structure and returns its "upperFluxBound" attribute.
 *
 * @param fbc the FbcReactionPlugin_t whose "upperFluxBound" attribute is sought.
 *
 * @return the "upperFluxBound" attribute of the given FbcReactionPlugin_t, as a pointer to a string.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
char *
FbcReactionPlugin_getUpperFluxBound(FbcSBasePlugin_t * fbc);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the given
 * FbcReactionPlugin_t structure's "upperFluxBound" attribute is set.
 *
 * @param fbc the FbcReactionPlugin_t structure to query.
 * 
 * @return @c 1 (true) if the "upperFluxBound" attribute of the given
 * FbcReactionPlugin_t structure is set, @c 0 (false) otherwise.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_isSetUpperFluxBound(FbcSBasePlugin_t * fbc);


/**
 * Sets the "upperFluxBound" attribute of the given FbcReactionPlugin_t to a copy of @p UpperFluxBound.
 *
 * @param fbc the FbcReactionPlugin_t structure to set.
 * @param UpperFluxBound the string to assign to the given FbcReactionPlugin_t's "upperFluxBound" attribute.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "upperFluxBound" attribute.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_setUpperFluxBound(FbcSBasePlugin_t * fbc, const char * UpperFluxBound);


/**
 * Unsets the "upperFluxBound" attribute of the given FbcReactionPlugin_t structure.
 *
 * @param fbc the FbcReactionPlugin_t structure to unset.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_unsetUpperFluxBound(FbcSBasePlugin_t * fbc);


/**
 * Takes a FbcReactionPlugin_t structure and returns its "lowerFluxBound" attribute.
 *
 * @param fbc the FbcReactionPlugin_t whose "lowerFluxBound" attribute is sought.
 *
 * @return the "lowerFluxBound" attribute of the given FbcReactionPlugin_t, as a pointer to a string.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
char *
FbcReactionPlugin_getLowerFluxBound(FbcSBasePlugin_t * fbc);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the given
 * FbcReactionPlugin_t structure's "lowerFluxBound" attribute is set.
 *
 * @param fbc the FbcReactionPlugin_t structure to query.
 * 
 * @return @c 1 (true) if the "lowerFluxBound" attribute attribute of the given
 * FbcReactionPlugin_t structure is set, @c 0 (false) otherwise.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_isSetLowerFluxBound(FbcSBasePlugin_t * fbc);


/**
 * Sets the "lowerFluxBound" attribute of the given FbcReactionPlugin_t to a copy of @p LowerFluxBound.
 *
 * @param fbc the FbcReactionPlugin_t structure to set.
 * @param LowerFluxBound the string to assign to the given FbcReactionPlugin_t's "lowerFluxBound" attribute.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "lowerFluxBound" attribute.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_setLowerFluxBound(FbcSBasePlugin_t * fbc, const char * LowerFluxBound);


/**
 * Unsets the "lowerFluxBound" attribute of the given FbcReactionPlugin_t structure.
 *
 * @param fbc the FbcReactionPlugin_t structure to unset.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_unsetLowerFluxBound(FbcSBasePlugin_t * fbc);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the given
 * FbcReactionPlugin_t structure's GeneProductAssociation is set.
 *
 * @param fbc the FbcReactionPlugin_t structure to query.
 * 
 * @return @c 1 (true) if the "geneProductAssopciation" elemen of the given
 * FbcReactionPlugin_t structure is set, @c 0 (false) otherwise.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_isSetGeneProductAssociation(FbcSBasePlugin_t * fbc);


/**
 * Takes a FbcReactionPlugin_t structure and returns its GeneProductAssociation_t.
 *
 * @param fbc the FbcReactionPlugin_t whose GeneProductAssociation_t is sought.
 *
 * @return the a pointer to the GeneProductAssociation_t of the given FbcReactionPlugin_t.
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
GeneProductAssociation_t*
FbcReactionPlugin_getGeneProductAssociation(FbcSBasePlugin_t * fbc);


/**
 * Takes a FbcReactionPlugin_t structure and sets its GeneProductAssociation_t.
 *
 * @param fbc the FbcReactionPlugin_t whose GeneProductAssociation_t is sought.
 * @param gpa a pointer to the GeneProductAssociation_t to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof FbcReactionPlugin_t
 */
LIBSBML_EXTERN
int
FbcReactionPlugin_setGeneProductAssociation(FbcSBasePlugin_t * fbc, 
                                            GeneProductAssociation_t* gpa);





END_C_DECLS

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* FbcReactionPlugin_H__ */


